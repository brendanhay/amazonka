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
-- Module      : Network.AWS.Pinpoint.UpdateVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing message template for messages that are sent through the voice channel.
module Network.AWS.Pinpoint.UpdateVoiceTemplate
  ( -- * Creating a Request
    updateVoiceTemplate,
    UpdateVoiceTemplate,

    -- * Request Lenses
    uvtVersion,
    uvtCreateNewVersion,
    uvtTemplateName,
    uvtVoiceTemplateRequest,

    -- * Destructuring the Response
    updateVoiceTemplateResponse,
    UpdateVoiceTemplateResponse,

    -- * Response Lenses
    uvtrsResponseStatus,
    uvtrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateVoiceTemplate' smart constructor.
data UpdateVoiceTemplate = UpdateVoiceTemplate'
  { _uvtVersion ::
      !(Maybe Text),
    _uvtCreateNewVersion :: !(Maybe Bool),
    _uvtTemplateName :: !Text,
    _uvtVoiceTemplateRequest :: !VoiceTemplateRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateVoiceTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvtVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'uvtCreateNewVersion' - Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
--
-- * 'uvtTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- * 'uvtVoiceTemplateRequest' - Undocumented member.
updateVoiceTemplate ::
  -- | 'uvtTemplateName'
  Text ->
  -- | 'uvtVoiceTemplateRequest'
  VoiceTemplateRequest ->
  UpdateVoiceTemplate
updateVoiceTemplate pTemplateName_ pVoiceTemplateRequest_ =
  UpdateVoiceTemplate'
    { _uvtVersion = Nothing,
      _uvtCreateNewVersion = Nothing,
      _uvtTemplateName = pTemplateName_,
      _uvtVoiceTemplateRequest = pVoiceTemplateRequest_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
uvtVersion :: Lens' UpdateVoiceTemplate (Maybe Text)
uvtVersion = lens _uvtVersion (\s a -> s {_uvtVersion = a})

-- | Specifies whether to save the updates as a new version of the message template. Valid values are: true, save the updates as a new version; and, false, save the updates to (overwrite) the latest existing version of the template. If you don't specify a value for this parameter, Amazon Pinpoint saves the updates to (overwrites) the latest existing version of the template. If you specify a value of true for this parameter, don't specify a value for the version parameter. Otherwise, an error will occur.
uvtCreateNewVersion :: Lens' UpdateVoiceTemplate (Maybe Bool)
uvtCreateNewVersion = lens _uvtCreateNewVersion (\s a -> s {_uvtCreateNewVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
uvtTemplateName :: Lens' UpdateVoiceTemplate Text
uvtTemplateName = lens _uvtTemplateName (\s a -> s {_uvtTemplateName = a})

-- | Undocumented member.
uvtVoiceTemplateRequest :: Lens' UpdateVoiceTemplate VoiceTemplateRequest
uvtVoiceTemplateRequest = lens _uvtVoiceTemplateRequest (\s a -> s {_uvtVoiceTemplateRequest = a})

instance AWSRequest UpdateVoiceTemplate where
  type Rs UpdateVoiceTemplate = UpdateVoiceTemplateResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateVoiceTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateVoiceTemplate

instance NFData UpdateVoiceTemplate

instance ToHeaders UpdateVoiceTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateVoiceTemplate where
  toJSON UpdateVoiceTemplate' {..} =
    object
      ( catMaybes
          [Just ("VoiceTemplateRequest" .= _uvtVoiceTemplateRequest)]
      )

instance ToPath UpdateVoiceTemplate where
  toPath UpdateVoiceTemplate' {..} =
    mconcat ["/v1/templates/", toBS _uvtTemplateName, "/voice"]

instance ToQuery UpdateVoiceTemplate where
  toQuery UpdateVoiceTemplate' {..} =
    mconcat
      [ "version" =: _uvtVersion,
        "create-new-version" =: _uvtCreateNewVersion
      ]

-- | /See:/ 'updateVoiceTemplateResponse' smart constructor.
data UpdateVoiceTemplateResponse = UpdateVoiceTemplateResponse'
  { _uvtrsResponseStatus ::
      !Int,
    _uvtrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateVoiceTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uvtrsResponseStatus' - -- | The response status code.
--
-- * 'uvtrsMessageBody' - Undocumented member.
updateVoiceTemplateResponse ::
  -- | 'uvtrsResponseStatus'
  Int ->
  -- | 'uvtrsMessageBody'
  MessageBody ->
  UpdateVoiceTemplateResponse
updateVoiceTemplateResponse pResponseStatus_ pMessageBody_ =
  UpdateVoiceTemplateResponse'
    { _uvtrsResponseStatus =
        pResponseStatus_,
      _uvtrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
uvtrsResponseStatus :: Lens' UpdateVoiceTemplateResponse Int
uvtrsResponseStatus = lens _uvtrsResponseStatus (\s a -> s {_uvtrsResponseStatus = a})

-- | Undocumented member.
uvtrsMessageBody :: Lens' UpdateVoiceTemplateResponse MessageBody
uvtrsMessageBody = lens _uvtrsMessageBody (\s a -> s {_uvtrsMessageBody = a})

instance NFData UpdateVoiceTemplateResponse
