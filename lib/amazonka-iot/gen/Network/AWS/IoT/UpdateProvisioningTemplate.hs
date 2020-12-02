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
-- Module      : Network.AWS.IoT.UpdateProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet provisioning template.
module Network.AWS.IoT.UpdateProvisioningTemplate
  ( -- * Creating a Request
    updateProvisioningTemplate,
    UpdateProvisioningTemplate,

    -- * Request Lenses
    uptPreProvisioningHook,
    uptEnabled,
    uptProvisioningRoleARN,
    uptDefaultVersionId,
    uptRemovePreProvisioningHook,
    uptDescription,
    uptTemplateName,

    -- * Destructuring the Response
    updateProvisioningTemplateResponse,
    UpdateProvisioningTemplateResponse,

    -- * Response Lenses
    uptrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { _uptPreProvisioningHook ::
      !(Maybe ProvisioningHook),
    _uptEnabled :: !(Maybe Bool),
    _uptProvisioningRoleARN ::
      !(Maybe Text),
    _uptDefaultVersionId :: !(Maybe Int),
    _uptRemovePreProvisioningHook ::
      !(Maybe Bool),
    _uptDescription :: !(Maybe Text),
    _uptTemplateName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProvisioningTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uptPreProvisioningHook' - Updates the pre-provisioning hook template.
--
-- * 'uptEnabled' - True to enable the fleet provisioning template, otherwise false.
--
-- * 'uptProvisioningRoleARN' - The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- * 'uptDefaultVersionId' - The ID of the default provisioning template version.
--
-- * 'uptRemovePreProvisioningHook' - Removes pre-provisioning hook template.
--
-- * 'uptDescription' - The description of the fleet provisioning template.
--
-- * 'uptTemplateName' - The name of the fleet provisioning template.
updateProvisioningTemplate ::
  -- | 'uptTemplateName'
  Text ->
  UpdateProvisioningTemplate
updateProvisioningTemplate pTemplateName_ =
  UpdateProvisioningTemplate'
    { _uptPreProvisioningHook = Nothing,
      _uptEnabled = Nothing,
      _uptProvisioningRoleARN = Nothing,
      _uptDefaultVersionId = Nothing,
      _uptRemovePreProvisioningHook = Nothing,
      _uptDescription = Nothing,
      _uptTemplateName = pTemplateName_
    }

-- | Updates the pre-provisioning hook template.
uptPreProvisioningHook :: Lens' UpdateProvisioningTemplate (Maybe ProvisioningHook)
uptPreProvisioningHook = lens _uptPreProvisioningHook (\s a -> s {_uptPreProvisioningHook = a})

-- | True to enable the fleet provisioning template, otherwise false.
uptEnabled :: Lens' UpdateProvisioningTemplate (Maybe Bool)
uptEnabled = lens _uptEnabled (\s a -> s {_uptEnabled = a})

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
uptProvisioningRoleARN :: Lens' UpdateProvisioningTemplate (Maybe Text)
uptProvisioningRoleARN = lens _uptProvisioningRoleARN (\s a -> s {_uptProvisioningRoleARN = a})

-- | The ID of the default provisioning template version.
uptDefaultVersionId :: Lens' UpdateProvisioningTemplate (Maybe Int)
uptDefaultVersionId = lens _uptDefaultVersionId (\s a -> s {_uptDefaultVersionId = a})

-- | Removes pre-provisioning hook template.
uptRemovePreProvisioningHook :: Lens' UpdateProvisioningTemplate (Maybe Bool)
uptRemovePreProvisioningHook = lens _uptRemovePreProvisioningHook (\s a -> s {_uptRemovePreProvisioningHook = a})

-- | The description of the fleet provisioning template.
uptDescription :: Lens' UpdateProvisioningTemplate (Maybe Text)
uptDescription = lens _uptDescription (\s a -> s {_uptDescription = a})

-- | The name of the fleet provisioning template.
uptTemplateName :: Lens' UpdateProvisioningTemplate Text
uptTemplateName = lens _uptTemplateName (\s a -> s {_uptTemplateName = a})

instance AWSRequest UpdateProvisioningTemplate where
  type
    Rs UpdateProvisioningTemplate =
      UpdateProvisioningTemplateResponse
  request = patchJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          UpdateProvisioningTemplateResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateProvisioningTemplate

instance NFData UpdateProvisioningTemplate

instance ToHeaders UpdateProvisioningTemplate where
  toHeaders = const mempty

instance ToJSON UpdateProvisioningTemplate where
  toJSON UpdateProvisioningTemplate' {..} =
    object
      ( catMaybes
          [ ("preProvisioningHook" .=) <$> _uptPreProvisioningHook,
            ("enabled" .=) <$> _uptEnabled,
            ("provisioningRoleArn" .=) <$> _uptProvisioningRoleARN,
            ("defaultVersionId" .=) <$> _uptDefaultVersionId,
            ("removePreProvisioningHook" .=) <$> _uptRemovePreProvisioningHook,
            ("description" .=) <$> _uptDescription
          ]
      )

instance ToPath UpdateProvisioningTemplate where
  toPath UpdateProvisioningTemplate' {..} =
    mconcat ["/provisioning-templates/", toBS _uptTemplateName]

instance ToQuery UpdateProvisioningTemplate where
  toQuery = const mempty

-- | /See:/ 'updateProvisioningTemplateResponse' smart constructor.
newtype UpdateProvisioningTemplateResponse = UpdateProvisioningTemplateResponse'
  { _uptrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uptrsResponseStatus' - -- | The response status code.
updateProvisioningTemplateResponse ::
  -- | 'uptrsResponseStatus'
  Int ->
  UpdateProvisioningTemplateResponse
updateProvisioningTemplateResponse pResponseStatus_ =
  UpdateProvisioningTemplateResponse'
    { _uptrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uptrsResponseStatus :: Lens' UpdateProvisioningTemplateResponse Int
uptrsResponseStatus = lens _uptrsResponseStatus (\s a -> s {_uptrsResponseStatus = a})

instance NFData UpdateProvisioningTemplateResponse
