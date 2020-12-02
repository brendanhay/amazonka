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
-- Module      : Network.AWS.Pinpoint.GetVoiceTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages that are sent through the voice channel.
module Network.AWS.Pinpoint.GetVoiceTemplate
  ( -- * Creating a Request
    getVoiceTemplate,
    GetVoiceTemplate,

    -- * Request Lenses
    gvtVersion,
    gvtTemplateName,

    -- * Destructuring the Response
    getVoiceTemplateResponse,
    GetVoiceTemplateResponse,

    -- * Response Lenses
    gvtrsResponseStatus,
    gvtrsVoiceTemplateResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getVoiceTemplate' smart constructor.
data GetVoiceTemplate = GetVoiceTemplate'
  { _gvtVersion ::
      !(Maybe Text),
    _gvtTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetVoiceTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvtVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'gvtTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
getVoiceTemplate ::
  -- | 'gvtTemplateName'
  Text ->
  GetVoiceTemplate
getVoiceTemplate pTemplateName_ =
  GetVoiceTemplate'
    { _gvtVersion = Nothing,
      _gvtTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
gvtVersion :: Lens' GetVoiceTemplate (Maybe Text)
gvtVersion = lens _gvtVersion (\s a -> s {_gvtVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
gvtTemplateName :: Lens' GetVoiceTemplate Text
gvtTemplateName = lens _gvtTemplateName (\s a -> s {_gvtTemplateName = a})

instance AWSRequest GetVoiceTemplate where
  type Rs GetVoiceTemplate = GetVoiceTemplateResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetVoiceTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetVoiceTemplate

instance NFData GetVoiceTemplate

instance ToHeaders GetVoiceTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetVoiceTemplate where
  toPath GetVoiceTemplate' {..} =
    mconcat ["/v1/templates/", toBS _gvtTemplateName, "/voice"]

instance ToQuery GetVoiceTemplate where
  toQuery GetVoiceTemplate' {..} = mconcat ["version" =: _gvtVersion]

-- | /See:/ 'getVoiceTemplateResponse' smart constructor.
data GetVoiceTemplateResponse = GetVoiceTemplateResponse'
  { _gvtrsResponseStatus ::
      !Int,
    _gvtrsVoiceTemplateResponse ::
      !VoiceTemplateResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetVoiceTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvtrsResponseStatus' - -- | The response status code.
--
-- * 'gvtrsVoiceTemplateResponse' - Undocumented member.
getVoiceTemplateResponse ::
  -- | 'gvtrsResponseStatus'
  Int ->
  -- | 'gvtrsVoiceTemplateResponse'
  VoiceTemplateResponse ->
  GetVoiceTemplateResponse
getVoiceTemplateResponse pResponseStatus_ pVoiceTemplateResponse_ =
  GetVoiceTemplateResponse'
    { _gvtrsResponseStatus =
        pResponseStatus_,
      _gvtrsVoiceTemplateResponse = pVoiceTemplateResponse_
    }

-- | -- | The response status code.
gvtrsResponseStatus :: Lens' GetVoiceTemplateResponse Int
gvtrsResponseStatus = lens _gvtrsResponseStatus (\s a -> s {_gvtrsResponseStatus = a})

-- | Undocumented member.
gvtrsVoiceTemplateResponse :: Lens' GetVoiceTemplateResponse VoiceTemplateResponse
gvtrsVoiceTemplateResponse = lens _gvtrsVoiceTemplateResponse (\s a -> s {_gvtrsVoiceTemplateResponse = a})

instance NFData GetVoiceTemplateResponse
