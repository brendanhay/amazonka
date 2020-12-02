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
-- Module      : Network.AWS.Pinpoint.DeleteVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the voice channel.
module Network.AWS.Pinpoint.DeleteVoiceTemplate
  ( -- * Creating a Request
    deleteVoiceTemplate,
    DeleteVoiceTemplate,

    -- * Request Lenses
    dvtVersion,
    dvtTemplateName,

    -- * Destructuring the Response
    deleteVoiceTemplateResponse,
    DeleteVoiceTemplateResponse,

    -- * Response Lenses
    dvtrsResponseStatus,
    dvtrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteVoiceTemplate' smart constructor.
data DeleteVoiceTemplate = DeleteVoiceTemplate'
  { _dvtVersion ::
      !(Maybe Text),
    _dvtTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteVoiceTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvtVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'dvtTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
deleteVoiceTemplate ::
  -- | 'dvtTemplateName'
  Text ->
  DeleteVoiceTemplate
deleteVoiceTemplate pTemplateName_ =
  DeleteVoiceTemplate'
    { _dvtVersion = Nothing,
      _dvtTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
dvtVersion :: Lens' DeleteVoiceTemplate (Maybe Text)
dvtVersion = lens _dvtVersion (\s a -> s {_dvtVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
dvtTemplateName :: Lens' DeleteVoiceTemplate Text
dvtTemplateName = lens _dvtTemplateName (\s a -> s {_dvtTemplateName = a})

instance AWSRequest DeleteVoiceTemplate where
  type Rs DeleteVoiceTemplate = DeleteVoiceTemplateResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeleteVoiceTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeleteVoiceTemplate

instance NFData DeleteVoiceTemplate

instance ToHeaders DeleteVoiceTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteVoiceTemplate where
  toPath DeleteVoiceTemplate' {..} =
    mconcat ["/v1/templates/", toBS _dvtTemplateName, "/voice"]

instance ToQuery DeleteVoiceTemplate where
  toQuery DeleteVoiceTemplate' {..} =
    mconcat ["version" =: _dvtVersion]

-- | /See:/ 'deleteVoiceTemplateResponse' smart constructor.
data DeleteVoiceTemplateResponse = DeleteVoiceTemplateResponse'
  { _dvtrsResponseStatus ::
      !Int,
    _dvtrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteVoiceTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvtrsResponseStatus' - -- | The response status code.
--
-- * 'dvtrsMessageBody' - Undocumented member.
deleteVoiceTemplateResponse ::
  -- | 'dvtrsResponseStatus'
  Int ->
  -- | 'dvtrsMessageBody'
  MessageBody ->
  DeleteVoiceTemplateResponse
deleteVoiceTemplateResponse pResponseStatus_ pMessageBody_ =
  DeleteVoiceTemplateResponse'
    { _dvtrsResponseStatus =
        pResponseStatus_,
      _dvtrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
dvtrsResponseStatus :: Lens' DeleteVoiceTemplateResponse Int
dvtrsResponseStatus = lens _dvtrsResponseStatus (\s a -> s {_dvtrsResponseStatus = a})

-- | Undocumented member.
dvtrsMessageBody :: Lens' DeleteVoiceTemplateResponse MessageBody
dvtrsMessageBody = lens _dvtrsMessageBody (\s a -> s {_dvtrsMessageBody = a})

instance NFData DeleteVoiceTemplateResponse
