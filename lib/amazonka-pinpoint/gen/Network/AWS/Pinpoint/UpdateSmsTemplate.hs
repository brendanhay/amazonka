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
-- Module      : Network.AWS.Pinpoint.UpdateSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the SMS channel.
module Network.AWS.Pinpoint.UpdateSmsTemplate
  ( -- * Creating a Request
    updateSmsTemplate,
    UpdateSmsTemplate,

    -- * Request Lenses
    ustVersion,
    ustCreateNewVersion,
    ustTemplateName,
    ustSMSTemplateRequest,

    -- * Destructuring the Response
    updateSmsTemplateResponse,
    UpdateSmsTemplateResponse,

    -- * Response Lenses
    ustrsResponseStatus,
    ustrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSmsTemplate' smart constructor.
data UpdateSmsTemplate = UpdateSmsTemplate'
  { _ustVersion ::
      !(Maybe Text),
    _ustCreateNewVersion :: !(Maybe Bool),
    _ustTemplateName :: !Text,
    _ustSMSTemplateRequest :: !SMSTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSmsTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ustVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'ustCreateNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- * 'ustTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'ustSMSTemplateRequest' - Undocumented member.
updateSmsTemplate ::
  -- | 'ustTemplateName'
  Text ->
  -- | 'ustSMSTemplateRequest'
  SMSTemplateRequest ->
  UpdateSmsTemplate
updateSmsTemplate pTemplateName_ pSMSTemplateRequest_ =
  UpdateSmsTemplate'
    { _ustVersion = Nothing,
      _ustCreateNewVersion = Nothing,
      _ustTemplateName = pTemplateName_,
      _ustSMSTemplateRequest = pSMSTemplateRequest_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
ustVersion :: Lens' UpdateSmsTemplate (Maybe Text)
ustVersion = lens _ustVersion (\s a -> s {_ustVersion = a})

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
ustCreateNewVersion :: Lens' UpdateSmsTemplate (Maybe Bool)
ustCreateNewVersion = lens _ustCreateNewVersion (\s a -> s {_ustCreateNewVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
ustTemplateName :: Lens' UpdateSmsTemplate Text
ustTemplateName = lens _ustTemplateName (\s a -> s {_ustTemplateName = a})

-- | Undocumented member.
ustSMSTemplateRequest :: Lens' UpdateSmsTemplate SMSTemplateRequest
ustSMSTemplateRequest = lens _ustSMSTemplateRequest (\s a -> s {_ustSMSTemplateRequest = a})

instance AWSRequest UpdateSmsTemplate where
  type Rs UpdateSmsTemplate = UpdateSmsTemplateResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateSmsTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateSmsTemplate

instance NFData UpdateSmsTemplate

instance ToHeaders UpdateSmsTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateSmsTemplate where
  toJSON UpdateSmsTemplate' {..} =
    object
      (catMaybes [Just ("SMSTemplateRequest" .= _ustSMSTemplateRequest)])

instance ToPath UpdateSmsTemplate where
  toPath UpdateSmsTemplate' {..} =
    mconcat ["/v1/templates/", toBS _ustTemplateName, "/sms"]

instance ToQuery UpdateSmsTemplate where
  toQuery UpdateSmsTemplate' {..} =
    mconcat
      [ "version" =: _ustVersion,
        "create-new-version" =: _ustCreateNewVersion
      ]

-- | /See:/ 'updateSmsTemplateResponse' smart constructor.
data UpdateSmsTemplateResponse = UpdateSmsTemplateResponse'
  { _ustrsResponseStatus ::
      !Int,
    _ustrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSmsTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ustrsResponseStatus' - -- | The response status code.
--
-- * 'ustrsMessageBody' - Undocumented member.
updateSmsTemplateResponse ::
  -- | 'ustrsResponseStatus'
  Int ->
  -- | 'ustrsMessageBody'
  MessageBody ->
  UpdateSmsTemplateResponse
updateSmsTemplateResponse pResponseStatus_ pMessageBody_ =
  UpdateSmsTemplateResponse'
    { _ustrsResponseStatus =
        pResponseStatus_,
      _ustrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
ustrsResponseStatus :: Lens' UpdateSmsTemplateResponse Int
ustrsResponseStatus = lens _ustrsResponseStatus (\s a -> s {_ustrsResponseStatus = a})

-- | Undocumented member.
ustrsMessageBody :: Lens' UpdateSmsTemplateResponse MessageBody
ustrsMessageBody = lens _ustrsMessageBody (\s a -> s {_ustrsMessageBody = a})

instance NFData UpdateSmsTemplateResponse
