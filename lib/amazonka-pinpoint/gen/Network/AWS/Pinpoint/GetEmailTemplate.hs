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
-- Module      : Network.AWS.Pinpoint.GetEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.GetEmailTemplate
  ( -- * Creating a Request
    getEmailTemplate,
    GetEmailTemplate,

    -- * Request Lenses
    getVersion,
    getTemplateName,

    -- * Destructuring the Response
    getEmailTemplateResponse,
    GetEmailTemplateResponse,

    -- * Response Lenses
    getrsResponseStatus,
    getrsEmailTemplateResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEmailTemplate' smart constructor.
data GetEmailTemplate = GetEmailTemplate'
  { _getVersion ::
      !(Maybe Text),
    _getTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'getTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
getEmailTemplate ::
  -- | 'getTemplateName'
  Text ->
  GetEmailTemplate
getEmailTemplate pTemplateName_ =
  GetEmailTemplate'
    { _getVersion = Nothing,
      _getTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
getVersion :: Lens' GetEmailTemplate (Maybe Text)
getVersion = lens _getVersion (\s a -> s {_getVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
getTemplateName :: Lens' GetEmailTemplate Text
getTemplateName = lens _getTemplateName (\s a -> s {_getTemplateName = a})

instance AWSRequest GetEmailTemplate where
  type Rs GetEmailTemplate = GetEmailTemplateResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetEmailTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetEmailTemplate

instance NFData GetEmailTemplate

instance ToHeaders GetEmailTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetEmailTemplate where
  toPath GetEmailTemplate' {..} =
    mconcat ["/v1/templates/", toBS _getTemplateName, "/email"]

instance ToQuery GetEmailTemplate where
  toQuery GetEmailTemplate' {..} = mconcat ["version" =: _getVersion]

-- | /See:/ 'getEmailTemplateResponse' smart constructor.
data GetEmailTemplateResponse = GetEmailTemplateResponse'
  { _getrsResponseStatus ::
      !Int,
    _getrsEmailTemplateResponse ::
      !EmailTemplateResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetEmailTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getrsResponseStatus' - -- | The response status code.
--
-- * 'getrsEmailTemplateResponse' - Undocumented member.
getEmailTemplateResponse ::
  -- | 'getrsResponseStatus'
  Int ->
  -- | 'getrsEmailTemplateResponse'
  EmailTemplateResponse ->
  GetEmailTemplateResponse
getEmailTemplateResponse pResponseStatus_ pEmailTemplateResponse_ =
  GetEmailTemplateResponse'
    { _getrsResponseStatus =
        pResponseStatus_,
      _getrsEmailTemplateResponse = pEmailTemplateResponse_
    }

-- | -- | The response status code.
getrsResponseStatus :: Lens' GetEmailTemplateResponse Int
getrsResponseStatus = lens _getrsResponseStatus (\s a -> s {_getrsResponseStatus = a})

-- | Undocumented member.
getrsEmailTemplateResponse :: Lens' GetEmailTemplateResponse EmailTemplateResponse
getrsEmailTemplateResponse = lens _getrsEmailTemplateResponse (\s a -> s {_getrsEmailTemplateResponse = a})

instance NFData GetEmailTemplateResponse
