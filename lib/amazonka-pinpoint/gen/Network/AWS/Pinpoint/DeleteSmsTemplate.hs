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
-- Module      : Network.AWS.Pinpoint.DeleteSmsTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the SMS channel.
module Network.AWS.Pinpoint.DeleteSmsTemplate
  ( -- * Creating a Request
    deleteSmsTemplate,
    DeleteSmsTemplate,

    -- * Request Lenses
    dstVersion,
    dstTemplateName,

    -- * Destructuring the Response
    deleteSmsTemplateResponse,
    DeleteSmsTemplateResponse,

    -- * Response Lenses
    dstrsResponseStatus,
    dstrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSmsTemplate' smart constructor.
data DeleteSmsTemplate = DeleteSmsTemplate'
  { _dstVersion ::
      !(Maybe Text),
    _dstTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSmsTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'dstTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
deleteSmsTemplate ::
  -- | 'dstTemplateName'
  Text ->
  DeleteSmsTemplate
deleteSmsTemplate pTemplateName_ =
  DeleteSmsTemplate'
    { _dstVersion = Nothing,
      _dstTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
dstVersion :: Lens' DeleteSmsTemplate (Maybe Text)
dstVersion = lens _dstVersion (\s a -> s {_dstVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
dstTemplateName :: Lens' DeleteSmsTemplate Text
dstTemplateName = lens _dstTemplateName (\s a -> s {_dstTemplateName = a})

instance AWSRequest DeleteSmsTemplate where
  type Rs DeleteSmsTemplate = DeleteSmsTemplateResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeleteSmsTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeleteSmsTemplate

instance NFData DeleteSmsTemplate

instance ToHeaders DeleteSmsTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteSmsTemplate where
  toPath DeleteSmsTemplate' {..} =
    mconcat ["/v1/templates/", toBS _dstTemplateName, "/sms"]

instance ToQuery DeleteSmsTemplate where
  toQuery DeleteSmsTemplate' {..} = mconcat ["version" =: _dstVersion]

-- | /See:/ 'deleteSmsTemplateResponse' smart constructor.
data DeleteSmsTemplateResponse = DeleteSmsTemplateResponse'
  { _dstrsResponseStatus ::
      !Int,
    _dstrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSmsTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dstrsResponseStatus' - -- | The response status code.
--
-- * 'dstrsMessageBody' - Undocumented member.
deleteSmsTemplateResponse ::
  -- | 'dstrsResponseStatus'
  Int ->
  -- | 'dstrsMessageBody'
  MessageBody ->
  DeleteSmsTemplateResponse
deleteSmsTemplateResponse pResponseStatus_ pMessageBody_ =
  DeleteSmsTemplateResponse'
    { _dstrsResponseStatus =
        pResponseStatus_,
      _dstrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
dstrsResponseStatus :: Lens' DeleteSmsTemplateResponse Int
dstrsResponseStatus = lens _dstrsResponseStatus (\s a -> s {_dstrsResponseStatus = a})

-- | Undocumented member.
dstrsMessageBody :: Lens' DeleteSmsTemplateResponse MessageBody
dstrsMessageBody = lens _dstrsMessageBody (\s a -> s {_dstrsMessageBody = a})

instance NFData DeleteSmsTemplateResponse
