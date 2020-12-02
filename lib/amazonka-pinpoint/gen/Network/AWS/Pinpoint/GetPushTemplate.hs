{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetPushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages that are sent through a push notification channel.
module Network.AWS.Pinpoint.GetPushTemplate
  ( -- * Creating a Request
    getPushTemplate,
    GetPushTemplate,

    -- * Request Lenses
    gptVersion,
    gptTemplateName,

    -- * Destructuring the Response
    getPushTemplateResponse,
    GetPushTemplateResponse,

    -- * Response Lenses
    gptrsResponseStatus,
    gptrsPushNotificationTemplateResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPushTemplate' smart constructor.
data GetPushTemplate = GetPushTemplate'
  { _gptVersion ::
      !(Maybe Text),
    _gptTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPushTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gptVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'gptTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
getPushTemplate ::
  -- | 'gptTemplateName'
  Text ->
  GetPushTemplate
getPushTemplate pTemplateName_ =
  GetPushTemplate'
    { _gptVersion = Nothing,
      _gptTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
gptVersion :: Lens' GetPushTemplate (Maybe Text)
gptVersion = lens _gptVersion (\s a -> s {_gptVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
gptTemplateName :: Lens' GetPushTemplate Text
gptTemplateName = lens _gptTemplateName (\s a -> s {_gptTemplateName = a})

instance AWSRequest GetPushTemplate where
  type Rs GetPushTemplate = GetPushTemplateResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetPushTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetPushTemplate

instance NFData GetPushTemplate

instance ToHeaders GetPushTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetPushTemplate where
  toPath GetPushTemplate' {..} =
    mconcat ["/v1/templates/", toBS _gptTemplateName, "/push"]

instance ToQuery GetPushTemplate where
  toQuery GetPushTemplate' {..} = mconcat ["version" =: _gptVersion]

-- | /See:/ 'getPushTemplateResponse' smart constructor.
data GetPushTemplateResponse = GetPushTemplateResponse'
  { _gptrsResponseStatus ::
      !Int,
    _gptrsPushNotificationTemplateResponse ::
      !PushNotificationTemplateResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetPushTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gptrsResponseStatus' - -- | The response status code.
--
-- * 'gptrsPushNotificationTemplateResponse' - Undocumented member.
getPushTemplateResponse ::
  -- | 'gptrsResponseStatus'
  Int ->
  -- | 'gptrsPushNotificationTemplateResponse'
  PushNotificationTemplateResponse ->
  GetPushTemplateResponse
getPushTemplateResponse
  pResponseStatus_
  pPushNotificationTemplateResponse_ =
    GetPushTemplateResponse'
      { _gptrsResponseStatus = pResponseStatus_,
        _gptrsPushNotificationTemplateResponse =
          pPushNotificationTemplateResponse_
      }

-- | -- | The response status code.
gptrsResponseStatus :: Lens' GetPushTemplateResponse Int
gptrsResponseStatus = lens _gptrsResponseStatus (\s a -> s {_gptrsResponseStatus = a})

-- | Undocumented member.
gptrsPushNotificationTemplateResponse :: Lens' GetPushTemplateResponse PushNotificationTemplateResponse
gptrsPushNotificationTemplateResponse = lens _gptrsPushNotificationTemplateResponse (\s a -> s {_gptrsPushNotificationTemplateResponse = a})

instance NFData GetPushTemplateResponse
