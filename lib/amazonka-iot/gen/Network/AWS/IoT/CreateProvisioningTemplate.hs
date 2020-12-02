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
-- Module      : Network.AWS.IoT.CreateProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplate
  ( -- * Creating a Request
    createProvisioningTemplate,
    CreateProvisioningTemplate,

    -- * Request Lenses
    cptPreProvisioningHook,
    cptEnabled,
    cptDescription,
    cptTags,
    cptTemplateName,
    cptTemplateBody,
    cptProvisioningRoleARN,

    -- * Destructuring the Response
    createProvisioningTemplateResponse,
    CreateProvisioningTemplateResponse,

    -- * Response Lenses
    cptrsTemplateName,
    cptrsDefaultVersionId,
    cptrsTemplateARN,
    cptrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProvisioningTemplate' smart constructor.
data CreateProvisioningTemplate = CreateProvisioningTemplate'
  { _cptPreProvisioningHook ::
      !(Maybe ProvisioningHook),
    _cptEnabled :: !(Maybe Bool),
    _cptDescription :: !(Maybe Text),
    _cptTags :: !(Maybe [Tag]),
    _cptTemplateName :: !Text,
    _cptTemplateBody :: !Text,
    _cptProvisioningRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProvisioningTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cptPreProvisioningHook' - Creates a pre-provisioning hook template.
--
-- * 'cptEnabled' - True to enable the fleet provisioning template, otherwise false.
--
-- * 'cptDescription' - The description of the fleet provisioning template.
--
-- * 'cptTags' - Metadata which can be used to manage the fleet provisioning template.
--
-- * 'cptTemplateName' - The name of the fleet provisioning template.
--
-- * 'cptTemplateBody' - The JSON formatted contents of the fleet provisioning template.
--
-- * 'cptProvisioningRoleARN' - The role ARN for the role associated with the fleet provisioning template. This IoT role grants permission to provision a device.
createProvisioningTemplate ::
  -- | 'cptTemplateName'
  Text ->
  -- | 'cptTemplateBody'
  Text ->
  -- | 'cptProvisioningRoleARN'
  Text ->
  CreateProvisioningTemplate
createProvisioningTemplate
  pTemplateName_
  pTemplateBody_
  pProvisioningRoleARN_ =
    CreateProvisioningTemplate'
      { _cptPreProvisioningHook = Nothing,
        _cptEnabled = Nothing,
        _cptDescription = Nothing,
        _cptTags = Nothing,
        _cptTemplateName = pTemplateName_,
        _cptTemplateBody = pTemplateBody_,
        _cptProvisioningRoleARN = pProvisioningRoleARN_
      }

-- | Creates a pre-provisioning hook template.
cptPreProvisioningHook :: Lens' CreateProvisioningTemplate (Maybe ProvisioningHook)
cptPreProvisioningHook = lens _cptPreProvisioningHook (\s a -> s {_cptPreProvisioningHook = a})

-- | True to enable the fleet provisioning template, otherwise false.
cptEnabled :: Lens' CreateProvisioningTemplate (Maybe Bool)
cptEnabled = lens _cptEnabled (\s a -> s {_cptEnabled = a})

-- | The description of the fleet provisioning template.
cptDescription :: Lens' CreateProvisioningTemplate (Maybe Text)
cptDescription = lens _cptDescription (\s a -> s {_cptDescription = a})

-- | Metadata which can be used to manage the fleet provisioning template.
cptTags :: Lens' CreateProvisioningTemplate [Tag]
cptTags = lens _cptTags (\s a -> s {_cptTags = a}) . _Default . _Coerce

-- | The name of the fleet provisioning template.
cptTemplateName :: Lens' CreateProvisioningTemplate Text
cptTemplateName = lens _cptTemplateName (\s a -> s {_cptTemplateName = a})

-- | The JSON formatted contents of the fleet provisioning template.
cptTemplateBody :: Lens' CreateProvisioningTemplate Text
cptTemplateBody = lens _cptTemplateBody (\s a -> s {_cptTemplateBody = a})

-- | The role ARN for the role associated with the fleet provisioning template. This IoT role grants permission to provision a device.
cptProvisioningRoleARN :: Lens' CreateProvisioningTemplate Text
cptProvisioningRoleARN = lens _cptProvisioningRoleARN (\s a -> s {_cptProvisioningRoleARN = a})

instance AWSRequest CreateProvisioningTemplate where
  type
    Rs CreateProvisioningTemplate =
      CreateProvisioningTemplateResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateResponse'
            <$> (x .?> "templateName")
            <*> (x .?> "defaultVersionId")
            <*> (x .?> "templateArn")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateProvisioningTemplate

instance NFData CreateProvisioningTemplate

instance ToHeaders CreateProvisioningTemplate where
  toHeaders = const mempty

instance ToJSON CreateProvisioningTemplate where
  toJSON CreateProvisioningTemplate' {..} =
    object
      ( catMaybes
          [ ("preProvisioningHook" .=) <$> _cptPreProvisioningHook,
            ("enabled" .=) <$> _cptEnabled,
            ("description" .=) <$> _cptDescription,
            ("tags" .=) <$> _cptTags,
            Just ("templateName" .= _cptTemplateName),
            Just ("templateBody" .= _cptTemplateBody),
            Just ("provisioningRoleArn" .= _cptProvisioningRoleARN)
          ]
      )

instance ToPath CreateProvisioningTemplate where
  toPath = const "/provisioning-templates"

instance ToQuery CreateProvisioningTemplate where
  toQuery = const mempty

-- | /See:/ 'createProvisioningTemplateResponse' smart constructor.
data CreateProvisioningTemplateResponse = CreateProvisioningTemplateResponse'
  { _cptrsTemplateName ::
      !(Maybe Text),
    _cptrsDefaultVersionId ::
      !(Maybe Int),
    _cptrsTemplateARN ::
      !(Maybe Text),
    _cptrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cptrsTemplateName' - The name of the fleet provisioning template.
--
-- * 'cptrsDefaultVersionId' - The default version of the fleet provisioning template.
--
-- * 'cptrsTemplateARN' - The ARN that identifies the provisioning template.
--
-- * 'cptrsResponseStatus' - -- | The response status code.
createProvisioningTemplateResponse ::
  -- | 'cptrsResponseStatus'
  Int ->
  CreateProvisioningTemplateResponse
createProvisioningTemplateResponse pResponseStatus_ =
  CreateProvisioningTemplateResponse'
    { _cptrsTemplateName = Nothing,
      _cptrsDefaultVersionId = Nothing,
      _cptrsTemplateARN = Nothing,
      _cptrsResponseStatus = pResponseStatus_
    }

-- | The name of the fleet provisioning template.
cptrsTemplateName :: Lens' CreateProvisioningTemplateResponse (Maybe Text)
cptrsTemplateName = lens _cptrsTemplateName (\s a -> s {_cptrsTemplateName = a})

-- | The default version of the fleet provisioning template.
cptrsDefaultVersionId :: Lens' CreateProvisioningTemplateResponse (Maybe Int)
cptrsDefaultVersionId = lens _cptrsDefaultVersionId (\s a -> s {_cptrsDefaultVersionId = a})

-- | The ARN that identifies the provisioning template.
cptrsTemplateARN :: Lens' CreateProvisioningTemplateResponse (Maybe Text)
cptrsTemplateARN = lens _cptrsTemplateARN (\s a -> s {_cptrsTemplateARN = a})

-- | -- | The response status code.
cptrsResponseStatus :: Lens' CreateProvisioningTemplateResponse Int
cptrsResponseStatus = lens _cptrsResponseStatus (\s a -> s {_cptrsResponseStatus = a})

instance NFData CreateProvisioningTemplateResponse
