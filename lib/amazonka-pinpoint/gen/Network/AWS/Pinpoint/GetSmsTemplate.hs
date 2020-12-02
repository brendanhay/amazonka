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
-- Module      : Network.AWS.Pinpoint.GetSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the content and settings of a message template for messages that are sent through the SMS channel.
module Network.AWS.Pinpoint.GetSmsTemplate
  ( -- * Creating a Request
    getSmsTemplate,
    GetSmsTemplate,

    -- * Request Lenses
    gstVersion,
    gstTemplateName,

    -- * Destructuring the Response
    getSmsTemplateResponse,
    GetSmsTemplateResponse,

    -- * Response Lenses
    gstrsResponseStatus,
    gstrsSMSTemplateResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getSmsTemplate' smart constructor.
data GetSmsTemplate = GetSmsTemplate'
  { _gstVersion :: !(Maybe Text),
    _gstTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSmsTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'gstTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
getSmsTemplate ::
  -- | 'gstTemplateName'
  Text ->
  GetSmsTemplate
getSmsTemplate pTemplateName_ =
  GetSmsTemplate'
    { _gstVersion = Nothing,
      _gstTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
gstVersion :: Lens' GetSmsTemplate (Maybe Text)
gstVersion = lens _gstVersion (\s a -> s {_gstVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
gstTemplateName :: Lens' GetSmsTemplate Text
gstTemplateName = lens _gstTemplateName (\s a -> s {_gstTemplateName = a})

instance AWSRequest GetSmsTemplate where
  type Rs GetSmsTemplate = GetSmsTemplateResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetSmsTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetSmsTemplate

instance NFData GetSmsTemplate

instance ToHeaders GetSmsTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetSmsTemplate where
  toPath GetSmsTemplate' {..} =
    mconcat ["/v1/templates/", toBS _gstTemplateName, "/sms"]

instance ToQuery GetSmsTemplate where
  toQuery GetSmsTemplate' {..} = mconcat ["version" =: _gstVersion]

-- | /See:/ 'getSmsTemplateResponse' smart constructor.
data GetSmsTemplateResponse = GetSmsTemplateResponse'
  { _gstrsResponseStatus ::
      !Int,
    _gstrsSMSTemplateResponse ::
      !SMSTemplateResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetSmsTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstrsResponseStatus' - -- | The response status code.
--
-- * 'gstrsSMSTemplateResponse' - Undocumented member.
getSmsTemplateResponse ::
  -- | 'gstrsResponseStatus'
  Int ->
  -- | 'gstrsSMSTemplateResponse'
  SMSTemplateResponse ->
  GetSmsTemplateResponse
getSmsTemplateResponse pResponseStatus_ pSMSTemplateResponse_ =
  GetSmsTemplateResponse'
    { _gstrsResponseStatus = pResponseStatus_,
      _gstrsSMSTemplateResponse = pSMSTemplateResponse_
    }

-- | -- | The response status code.
gstrsResponseStatus :: Lens' GetSmsTemplateResponse Int
gstrsResponseStatus = lens _gstrsResponseStatus (\s a -> s {_gstrsResponseStatus = a})

-- | Undocumented member.
gstrsSMSTemplateResponse :: Lens' GetSmsTemplateResponse SMSTemplateResponse
gstrsSMSTemplateResponse = lens _gstrsSMSTemplateResponse (\s a -> s {_gstrsSMSTemplateResponse = a})

instance NFData GetSmsTemplateResponse
