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
-- Module      : Network.AWS.Pinpoint.DeleteEmailTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a message template for messages that were sent through the email channel.
module Network.AWS.Pinpoint.DeleteEmailTemplate
  ( -- * Creating a Request
    deleteEmailTemplate,
    DeleteEmailTemplate,

    -- * Request Lenses
    detVersion,
    detTemplateName,

    -- * Destructuring the Response
    deleteEmailTemplateResponse,
    DeleteEmailTemplateResponse,

    -- * Response Lenses
    detrsResponseStatus,
    detrsMessageBody,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteEmailTemplate' smart constructor.
data DeleteEmailTemplate = DeleteEmailTemplate'
  { _detVersion ::
      !(Maybe Text),
    _detTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEmailTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detVersion' - The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
--
-- * 'detTemplateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
deleteEmailTemplate ::
  -- | 'detTemplateName'
  Text ->
  DeleteEmailTemplate
deleteEmailTemplate pTemplateName_ =
  DeleteEmailTemplate'
    { _detVersion = Nothing,
      _detTemplateName = pTemplateName_
    }

-- | The unique identifier for the version of the message template to update, retrieve information about, or delete. To retrieve identifiers and other information for all the versions of a template, use the <link>Template Versions resource. If specified, this value must match the identifier for an existing template version. If specified for an update operation, this value must match the identifier for the latest existing version of the template. This restriction helps ensure that race conditions don't occur. If you don't specify a value for this parameter, Amazon Pinpoint does the following:     * For a get operation, retrieves information about the active version of the template.     * For an update operation, saves the updates to (overwrites) the latest existing version of the template, if the create-new-version parameter isn't used or is set to false.     * For a delete operation, deletes the template, including all versions of the template.
detVersion :: Lens' DeleteEmailTemplate (Maybe Text)
detVersion = lens _detVersion (\s a -> s {_detVersion = a})

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
detTemplateName :: Lens' DeleteEmailTemplate Text
detTemplateName = lens _detTemplateName (\s a -> s {_detTemplateName = a})

instance AWSRequest DeleteEmailTemplate where
  type Rs DeleteEmailTemplate = DeleteEmailTemplateResponse
  request = delete pinpoint
  response =
    receiveJSON
      ( \s h x ->
          DeleteEmailTemplateResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable DeleteEmailTemplate

instance NFData DeleteEmailTemplate

instance ToHeaders DeleteEmailTemplate where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteEmailTemplate where
  toPath DeleteEmailTemplate' {..} =
    mconcat ["/v1/templates/", toBS _detTemplateName, "/email"]

instance ToQuery DeleteEmailTemplate where
  toQuery DeleteEmailTemplate' {..} =
    mconcat ["version" =: _detVersion]

-- | /See:/ 'deleteEmailTemplateResponse' smart constructor.
data DeleteEmailTemplateResponse = DeleteEmailTemplateResponse'
  { _detrsResponseStatus ::
      !Int,
    _detrsMessageBody :: !MessageBody
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteEmailTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsResponseStatus' - -- | The response status code.
--
-- * 'detrsMessageBody' - Undocumented member.
deleteEmailTemplateResponse ::
  -- | 'detrsResponseStatus'
  Int ->
  -- | 'detrsMessageBody'
  MessageBody ->
  DeleteEmailTemplateResponse
deleteEmailTemplateResponse pResponseStatus_ pMessageBody_ =
  DeleteEmailTemplateResponse'
    { _detrsResponseStatus =
        pResponseStatus_,
      _detrsMessageBody = pMessageBody_
    }

-- | -- | The response status code.
detrsResponseStatus :: Lens' DeleteEmailTemplateResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\s a -> s {_detrsResponseStatus = a})

-- | Undocumented member.
detrsMessageBody :: Lens' DeleteEmailTemplateResponse MessageBody
detrsMessageBody = lens _detrsMessageBody (\s a -> s {_detrsMessageBody = a})

instance NFData DeleteEmailTemplateResponse
