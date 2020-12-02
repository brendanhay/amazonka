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
-- Module      : Network.AWS.Pinpoint.UpdateEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the email channel.
module Network.AWS.Pinpoint.UpdateEmailTemplate
  ( -- * Creating a Request
    updateEmailTemplate,
    UpdateEmailTemplate,

    -- * Request Lenses
    uetVersion,
    uetCreateNewVersion,
    uetTemplateName,
    uetEmailTemplateRequest,

    -- * Destructuring the Response
    updateEmailTemplateResponse,
    UpdateEmailTemplateResponse,

    -- * Response Lenses
    uetrsResponseStatus,
    uetrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEmailTemplate' smart constructor.
data UpdateEmailTemplate = UpdateEmailTemplate'
  { _uetVersion ::
      !(Maybe Text),
    _uetCreateNewVersion :: !(Maybe Bool),
    _uetTemplateName :: !Text,
    _uetEmailTemplateRequest :: !EmailTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uetVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'uetCreateNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- * 'uetTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'uetEmailTemplateRequest' - Undocumented member.
updateEmailTemplate ::
  -- | 'uetTemplateName'
  Text ->
  -- | 'uetEmailTemplateRequest'
  EmailTemplateRequest ->
  UpdateEmailTemplate
updateEmailTemplate pTemplateName_ pEmailTemplateRequest_ =
  UpdateEmailTemplate'
    { _uetVersion = Nothing,
      _uetCreateNewVersion = Nothing,
      _uetTemplateName = pTemplateName_,
      _uetEmailTemplateRequest = pEmailTemplateRequest_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
uetVersion :: Lens' UpdateEmailTemplate (Maybe Text)
uetVersion = lens _uetVersion (\s a -> s {_uetVersion = a})

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
uetCreateNewVersion :: Lens' UpdateEmailTemplate (Maybe Bool)
uetCreateNewVersion = lens _uetCreateNewVersion (\s a -> s {_uetCreateNewVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
uetTemplateName :: Lens' UpdateEmailTemplate Text
uetTemplateName = lens _uetTemplateName (\s a -> s {_uetTemplateName = a})

-- | Undocumented member.
uetEmailTemplateRequest :: Lens' UpdateEmailTemplate EmailTemplateRequest
uetEmailTemplateRequest = lens _uetEmailTemplateRequest (\s a -> s {_uetEmailTemplateRequest = a})

instance AWSRequest UpdateEmailTemplate where
  type Rs UpdateEmailTemplate = UpdateEmailTemplateResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateEmailTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateEmailTemplate

instance NFData UpdateEmailTemplate

instance ToHeaders UpdateEmailTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateEmailTemplate where
  toJSON UpdateEmailTemplate' {..} =
    object
      ( catMaybes
          [Just ("EmailTemplateRequest" .= _uetEmailTemplateRequest)]
      )

instance ToPath UpdateEmailTemplate where
  toPath UpdateEmailTemplate' {..} =
    mconcat ["/v1/templates/", toBS _uetTemplateName, "/email"]

instance ToQuery UpdateEmailTemplate where
  toQuery UpdateEmailTemplate' {..} =
    mconcat
      [ "version" =: _uetVersion,
        "create-new-version" =: _uetCreateNewVersion
      ]

-- | /See:/ 'updateEmailTemplateResponse' smart constructor.
data UpdateEmailTemplateResponse = UpdateEmailTemplateResponse'
  { _uetrsResponseStatus ::
      !Int,
    _uetrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateEmailTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uetrsResponseStatus' - -- | The response status code.
--
-- * 'uetrsMessageBody' - Undocumented member.
updateEmailTemplateResponse ::
  -- | 'uetrsResponseStatus'
  Int ->
  -- | 'uetrsMessageBody'
  MessageBody ->
  UpdateEmailTemplateResponse
updateEmailTemplateResponse pResponseStatus_ pMessageBody_ =
  UpdateEmailTemplateResponse'
    { _uetrsResponseStatus =
        pResponseStatus_,
      _uetrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
uetrsResponseStatus :: Lens' UpdateEmailTemplateResponse Int
uetrsResponseStatus = lens _uetrsResponseStatus (\s a -> s {_uetrsResponseStatus = a})

-- | Undocumented member.
uetrsMessageBody :: Lens' UpdateEmailTemplateResponse MessageBody
uetrsMessageBody = lens _uetrsMessageBody (\s a -> s {_uetrsMessageBody = a})

instance NFData UpdateEmailTemplateResponse
