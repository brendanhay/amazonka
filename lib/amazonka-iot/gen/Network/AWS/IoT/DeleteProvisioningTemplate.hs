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
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template.
module Network.AWS.IoT.DeleteProvisioningTemplate
  ( -- * Creating a Request
    deleteProvisioningTemplate,
    DeleteProvisioningTemplate,

    -- * Request Lenses
    dTemplateName,

    -- * Destructuring the Response
    deleteProvisioningTemplateResponse,
    DeleteProvisioningTemplateResponse,

    -- * Response Lenses
    delrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProvisioningTemplate' smart constructor.
newtype DeleteProvisioningTemplate = DeleteProvisioningTemplate'
  { _dTemplateName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProvisioningTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTemplateName' - The name of the fleet provision template to delete.
deleteProvisioningTemplate ::
  -- | 'dTemplateName'
  Text ->
  DeleteProvisioningTemplate
deleteProvisioningTemplate pTemplateName_ =
  DeleteProvisioningTemplate' {_dTemplateName = pTemplateName_}

-- | The name of the fleet provision template to delete.
dTemplateName :: Lens' DeleteProvisioningTemplate Text
dTemplateName = lens _dTemplateName (\s a -> s {_dTemplateName = a})

instance AWSRequest DeleteProvisioningTemplate where
  type
    Rs DeleteProvisioningTemplate =
      DeleteProvisioningTemplateResponse
  request = delete ioT
  response =
    receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteProvisioningTemplate

instance NFData DeleteProvisioningTemplate

instance ToHeaders DeleteProvisioningTemplate where
  toHeaders = const mempty

instance ToPath DeleteProvisioningTemplate where
  toPath DeleteProvisioningTemplate' {..} =
    mconcat ["/provisioning-templates/", toBS _dTemplateName]

instance ToQuery DeleteProvisioningTemplate where
  toQuery = const mempty

-- | /See:/ 'deleteProvisioningTemplateResponse' smart constructor.
newtype DeleteProvisioningTemplateResponse = DeleteProvisioningTemplateResponse'
  { _delrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteProvisioningTemplateResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteProvisioningTemplateResponse
deleteProvisioningTemplateResponse pResponseStatus_ =
  DeleteProvisioningTemplateResponse'
    { _delrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteProvisioningTemplateResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteProvisioningTemplateResponse
