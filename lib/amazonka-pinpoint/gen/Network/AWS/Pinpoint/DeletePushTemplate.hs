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
-- Module      : Network.AWS.Pinpoint.DeletePushTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through a push notification channel.
module Network.AWS.Pinpoint.DeletePushTemplate
  ( -- * Creating a Request
    deletePushTemplate,
    DeletePushTemplate,

    -- * Request Lenses
    dptVersion,
    dptTemplateName,

    -- * Destructuring the Response
    deletePushTemplateResponse,
    DeletePushTemplateResponse,

    -- * Response Lenses
    dptrsResponseStatus,
    dptrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePushTemplate' smart constructor.
data DeletePushTemplate = DeletePushTemplate'
  { _dptVersion ::
      !(Maybe Text),
    _dptTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePushTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'dptTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
deletePushTemplate ::
  -- | 'dptTemplateName'
  Text ->
  DeletePushTemplate
deletePushTemplate pTemplateName_ =
  DeletePushTemplate'
    { _dptVersion = Nothing,
      _dptTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
dptVersion :: Lens' DeletePushTemplate (Maybe Text)
dptVersion = lens _dptVersion (\s a -> s {_dptVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
dptTemplateName :: Lens' DeletePushTemplate Text
dptTemplateName = lens _dptTemplateName (\s a -> s {_dptTemplateName = a})

instance AWSRequest DeletePushTemplate where
  type Rs DeletePushTemplate = DeletePushTemplateResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeletePushTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeletePushTemplate

instance NFData DeletePushTemplate

instance ToHeaders DeletePushTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeletePushTemplate where
  toPath DeletePushTemplate' {..} =
    mconcat ["/v1/templates/", toBS _dptTemplateName, "/push"]

instance ToQuery DeletePushTemplate where
  toQuery DeletePushTemplate' {..} =
    mconcat ["version" =: _dptVersion]

-- | /See:/ 'deletePushTemplateResponse' smart constructor.
data DeletePushTemplateResponse = DeletePushTemplateResponse'
  { _dptrsResponseStatus ::
      !Int,
    _dptrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeletePushTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptrsResponseStatus' - -- | The response status code.
--
-- * 'dptrsMessageBody' - Undocumented member.
deletePushTemplateResponse ::
  -- | 'dptrsResponseStatus'
  Int ->
  -- | 'dptrsMessageBody'
  MessageBody ->
  DeletePushTemplateResponse
deletePushTemplateResponse pResponseStatus_ pMessageBody_ =
  DeletePushTemplateResponse'
    { _dptrsResponseStatus =
        pResponseStatus_,
      _dptrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
dptrsResponseStatus :: Lens' DeletePushTemplateResponse Int
dptrsResponseStatus = lens _dptrsResponseStatus (\s a -> s {_dptrsResponseStatus = a})

-- | Undocumented member.
dptrsMessageBody :: Lens' DeletePushTemplateResponse MessageBody
dptrsMessageBody = lens _dptrsMessageBody (\s a -> s {_dptrsMessageBody = a})

instance NFData DeletePushTemplateResponse
