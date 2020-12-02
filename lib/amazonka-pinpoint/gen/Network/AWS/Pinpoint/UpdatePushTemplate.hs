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
-- Module      : Network.AWS.Pinpoint.UpdatePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through a push notification channel.
module Network.AWS.Pinpoint.UpdatePushTemplate
  ( -- * Creating a Request
    updatePushTemplate,
    UpdatePushTemplate,

    -- * Request Lenses
    uptVersion,
    uptCreateNewVersion,
    uptTemplateName,
    uptPushNotificationTemplateRequest,

    -- * Destructuring the Response
    updatePushTemplateResponse,
    UpdatePushTemplateResponse,

    -- * Response Lenses
    uptrsResponseStatus,
    uptrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updatePushTemplate' smart constructor.
data UpdatePushTemplate = UpdatePushTemplate'
  { _uptVersion ::
      !(Maybe Text),
    _uptCreateNewVersion :: !(Maybe Bool),
    _uptTemplateName :: !Text,
    _uptPushNotificationTemplateRequest ::
      !PushNotificationTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePushTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uptVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'uptCreateNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- * 'uptTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'uptPushNotificationTemplateRequest' - Undocumented member.
updatePushTemplate ::
  -- | 'uptTemplateName'
  Text ->
  -- | 'uptPushNotificationTemplateRequest'
  PushNotificationTemplateRequest ->
  UpdatePushTemplate
updatePushTemplate pTemplateName_ pPushNotificationTemplateRequest_ =
  UpdatePushTemplate'
    { _uptVersion = Nothing,
      _uptCreateNewVersion = Nothing,
      _uptTemplateName = pTemplateName_,
      _uptPushNotificationTemplateRequest =
        pPushNotificationTemplateRequest_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
uptVersion :: Lens' UpdatePushTemplate (Maybe Text)
uptVersion = lens _uptVersion (\s a -> s {_uptVersion = a})

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
uptCreateNewVersion :: Lens' UpdatePushTemplate (Maybe Bool)
uptCreateNewVersion = lens _uptCreateNewVersion (\s a -> s {_uptCreateNewVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
uptTemplateName :: Lens' UpdatePushTemplate Text
uptTemplateName = lens _uptTemplateName (\s a -> s {_uptTemplateName = a})

-- | Undocumented member.
uptPushNotificationTemplateRequest :: Lens' UpdatePushTemplate PushNotificationTemplateRequest
uptPushNotificationTemplateRequest = lens _uptPushNotificationTemplateRequest (\s a -> s {_uptPushNotificationTemplateRequest = a})

instance AWSRequest UpdatePushTemplate where
  type Rs UpdatePushTemplate = UpdatePushTemplateResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdatePushTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdatePushTemplate

instance NFData UpdatePushTemplate

instance ToHeaders UpdatePushTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdatePushTemplate where
  toJSON UpdatePushTemplate' {..} =
    object
      ( catMaybes
          [ Just
              ( "PushNotificationTemplateRequest"
                  .= _uptPushNotificationTemplateRequest
              )
          ]
      )

instance ToPath UpdatePushTemplate where
  toPath UpdatePushTemplate' {..} =
    mconcat ["/v1/templates/", toBS _uptTemplateName, "/push"]

instance ToQuery UpdatePushTemplate where
  toQuery UpdatePushTemplate' {..} =
    mconcat
      [ "version" =: _uptVersion,
        "create-new-version" =: _uptCreateNewVersion
      ]

-- | /See:/ 'updatePushTemplateResponse' smart constructor.
data UpdatePushTemplateResponse = UpdatePushTemplateResponse'
  { _uptrsResponseStatus ::
      !Int,
    _uptrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdatePushTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uptrsResponseStatus' - -- | The response status code.
--
-- * 'uptrsMessageBody' - Undocumented member.
updatePushTemplateResponse ::
  -- | 'uptrsResponseStatus'
  Int ->
  -- | 'uptrsMessageBody'
  MessageBody ->
  UpdatePushTemplateResponse
updatePushTemplateResponse pResponseStatus_ pMessageBody_ =
  UpdatePushTemplateResponse'
    { _uptrsResponseStatus =
        pResponseStatus_,
      _uptrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
uptrsResponseStatus :: Lens' UpdatePushTemplateResponse Int
uptrsResponseStatus = lens _uptrsResponseStatus (\s a -> s {_uptrsResponseStatus = a})

-- | Undocumented member.
uptrsMessageBody :: Lens' UpdatePushTemplateResponse MessageBody
uptrsMessageBody = lens _uptrsMessageBody (\s a -> s {_uptrsMessageBody = a})

instance NFData UpdatePushTemplateResponse
