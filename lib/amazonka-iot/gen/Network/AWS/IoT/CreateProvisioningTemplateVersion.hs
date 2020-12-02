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
-- Module      : Network.AWS.IoT.CreateProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplateVersion
  ( -- * Creating a Request
    createProvisioningTemplateVersion,
    CreateProvisioningTemplateVersion,

    -- * Request Lenses
    cptvSetAsDefault,
    cptvTemplateName,
    cptvTemplateBody,

    -- * Destructuring the Response
    createProvisioningTemplateVersionResponse,
    CreateProvisioningTemplateVersionResponse,

    -- * Response Lenses
    cptvrsVersionId,
    cptvrsTemplateName,
    cptvrsTemplateARN,
    cptvrsIsDefaultVersion,
    cptvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProvisioningTemplateVersion' smart constructor.
data CreateProvisioningTemplateVersion = CreateProvisioningTemplateVersion'
  { _cptvSetAsDefault ::
      !(Maybe Bool),
    _cptvTemplateName ::
      !Text,
    _cptvTemplateBody ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProvisioningTemplateVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cptvSetAsDefault' - Sets a fleet provision template version as the default version.
--
-- * 'cptvTemplateName' - The name of the fleet provisioning template.
--
-- * 'cptvTemplateBody' - The JSON formatted contents of the fleet provisioning template.
createProvisioningTemplateVersion ::
  -- | 'cptvTemplateName'
  Text ->
  -- | 'cptvTemplateBody'
  Text ->
  CreateProvisioningTemplateVersion
createProvisioningTemplateVersion pTemplateName_ pTemplateBody_ =
  CreateProvisioningTemplateVersion'
    { _cptvSetAsDefault = Nothing,
      _cptvTemplateName = pTemplateName_,
      _cptvTemplateBody = pTemplateBody_
    }

-- | Sets a fleet provision template version as the default version.
cptvSetAsDefault :: Lens' CreateProvisioningTemplateVersion (Maybe Bool)
cptvSetAsDefault = lens _cptvSetAsDefault (\s a -> s {_cptvSetAsDefault = a})

-- | The name of the fleet provisioning template.
cptvTemplateName :: Lens' CreateProvisioningTemplateVersion Text
cptvTemplateName = lens _cptvTemplateName (\s a -> s {_cptvTemplateName = a})

-- | The JSON formatted contents of the fleet provisioning template.
cptvTemplateBody :: Lens' CreateProvisioningTemplateVersion Text
cptvTemplateBody = lens _cptvTemplateBody (\s a -> s {_cptvTemplateBody = a})

instance AWSRequest CreateProvisioningTemplateVersion where
  type
    Rs CreateProvisioningTemplateVersion =
      CreateProvisioningTemplateVersionResponse
  request = postJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateVersionResponse'
            <$> (x .?> "versionId")
            <*> (x .?> "templateName")
            <*> (x .?> "templateArn")
            <*> (x .?> "isDefaultVersion")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateProvisioningTemplateVersion

instance NFData CreateProvisioningTemplateVersion

instance ToHeaders CreateProvisioningTemplateVersion where
  toHeaders = const mempty

instance ToJSON CreateProvisioningTemplateVersion where
  toJSON CreateProvisioningTemplateVersion' {..} =
    object (catMaybes [Just ("templateBody" .= _cptvTemplateBody)])

instance ToPath CreateProvisioningTemplateVersion where
  toPath CreateProvisioningTemplateVersion' {..} =
    mconcat
      ["/provisioning-templates/", toBS _cptvTemplateName, "/versions"]

instance ToQuery CreateProvisioningTemplateVersion where
  toQuery CreateProvisioningTemplateVersion' {..} =
    mconcat ["setAsDefault" =: _cptvSetAsDefault]

-- | /See:/ 'createProvisioningTemplateVersionResponse' smart constructor.
data CreateProvisioningTemplateVersionResponse = CreateProvisioningTemplateVersionResponse'
  { _cptvrsVersionId ::
      !( Maybe
           Int
       ),
    _cptvrsTemplateName ::
      !( Maybe
           Text
       ),
    _cptvrsTemplateARN ::
      !( Maybe
           Text
       ),
    _cptvrsIsDefaultVersion ::
      !( Maybe
           Bool
       ),
    _cptvrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CreateProvisioningTemplateVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cptvrsVersionId' - The version of the fleet provisioning template.
--
-- * 'cptvrsTemplateName' - The name of the fleet provisioning template.
--
-- * 'cptvrsTemplateARN' - The ARN that identifies the provisioning template.
--
-- * 'cptvrsIsDefaultVersion' - True if the fleet provisioning template version is the default version, otherwise false.
--
-- * 'cptvrsResponseStatus' - -- | The response status code.
createProvisioningTemplateVersionResponse ::
  -- | 'cptvrsResponseStatus'
  Int ->
  CreateProvisioningTemplateVersionResponse
createProvisioningTemplateVersionResponse pResponseStatus_ =
  CreateProvisioningTemplateVersionResponse'
    { _cptvrsVersionId =
        Nothing,
      _cptvrsTemplateName = Nothing,
      _cptvrsTemplateARN = Nothing,
      _cptvrsIsDefaultVersion = Nothing,
      _cptvrsResponseStatus = pResponseStatus_
    }

-- | The version of the fleet provisioning template.
cptvrsVersionId :: Lens' CreateProvisioningTemplateVersionResponse (Maybe Int)
cptvrsVersionId = lens _cptvrsVersionId (\s a -> s {_cptvrsVersionId = a})

-- | The name of the fleet provisioning template.
cptvrsTemplateName :: Lens' CreateProvisioningTemplateVersionResponse (Maybe Text)
cptvrsTemplateName = lens _cptvrsTemplateName (\s a -> s {_cptvrsTemplateName = a})

-- | The ARN that identifies the provisioning template.
cptvrsTemplateARN :: Lens' CreateProvisioningTemplateVersionResponse (Maybe Text)
cptvrsTemplateARN = lens _cptvrsTemplateARN (\s a -> s {_cptvrsTemplateARN = a})

-- | True if the fleet provisioning template version is the default version, otherwise false.
cptvrsIsDefaultVersion :: Lens' CreateProvisioningTemplateVersionResponse (Maybe Bool)
cptvrsIsDefaultVersion = lens _cptvrsIsDefaultVersion (\s a -> s {_cptvrsIsDefaultVersion = a})

-- | -- | The response status code.
cptvrsResponseStatus :: Lens' CreateProvisioningTemplateVersionResponse Int
cptvrsResponseStatus = lens _cptvrsResponseStatus (\s a -> s {_cptvrsResponseStatus = a})

instance NFData CreateProvisioningTemplateVersionResponse
