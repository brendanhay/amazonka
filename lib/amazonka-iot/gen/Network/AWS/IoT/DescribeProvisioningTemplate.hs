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
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template.
module Network.AWS.IoT.DescribeProvisioningTemplate
  ( -- * Creating a Request
    describeProvisioningTemplate,
    DescribeProvisioningTemplate,

    -- * Request Lenses
    dptTemplateName,

    -- * Destructuring the Response
    describeProvisioningTemplateResponse,
    DescribeProvisioningTemplateResponse,

    -- * Response Lenses
    dptrsLastModifiedDate,
    dptrsTemplateName,
    dptrsPreProvisioningHook,
    dptrsEnabled,
    dptrsProvisioningRoleARN,
    dptrsDefaultVersionId,
    dptrsCreationDate,
    dptrsTemplateARN,
    dptrsTemplateBody,
    dptrsDescription,
    dptrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeProvisioningTemplate' smart constructor.
newtype DescribeProvisioningTemplate = DescribeProvisioningTemplate'
  { _dptTemplateName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProvisioningTemplate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptTemplateName' - The name of the fleet provisioning template.
describeProvisioningTemplate ::
  -- | 'dptTemplateName'
  Text ->
  DescribeProvisioningTemplate
describeProvisioningTemplate pTemplateName_ =
  DescribeProvisioningTemplate' {_dptTemplateName = pTemplateName_}

-- | The name of the fleet provisioning template.
dptTemplateName :: Lens' DescribeProvisioningTemplate Text
dptTemplateName = lens _dptTemplateName (\s a -> s {_dptTemplateName = a})

instance AWSRequest DescribeProvisioningTemplate where
  type
    Rs DescribeProvisioningTemplate =
      DescribeProvisioningTemplateResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateResponse'
            <$> (x .?> "lastModifiedDate")
            <*> (x .?> "templateName")
            <*> (x .?> "preProvisioningHook")
            <*> (x .?> "enabled")
            <*> (x .?> "provisioningRoleArn")
            <*> (x .?> "defaultVersionId")
            <*> (x .?> "creationDate")
            <*> (x .?> "templateArn")
            <*> (x .?> "templateBody")
            <*> (x .?> "description")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeProvisioningTemplate

instance NFData DescribeProvisioningTemplate

instance ToHeaders DescribeProvisioningTemplate where
  toHeaders = const mempty

instance ToPath DescribeProvisioningTemplate where
  toPath DescribeProvisioningTemplate' {..} =
    mconcat ["/provisioning-templates/", toBS _dptTemplateName]

instance ToQuery DescribeProvisioningTemplate where
  toQuery = const mempty

-- | /See:/ 'describeProvisioningTemplateResponse' smart constructor.
data DescribeProvisioningTemplateResponse = DescribeProvisioningTemplateResponse'
  { _dptrsLastModifiedDate ::
      !(Maybe POSIX),
    _dptrsTemplateName ::
      !(Maybe Text),
    _dptrsPreProvisioningHook ::
      !( Maybe
           ProvisioningHook
       ),
    _dptrsEnabled ::
      !(Maybe Bool),
    _dptrsProvisioningRoleARN ::
      !(Maybe Text),
    _dptrsDefaultVersionId ::
      !(Maybe Int),
    _dptrsCreationDate ::
      !(Maybe POSIX),
    _dptrsTemplateARN ::
      !(Maybe Text),
    _dptrsTemplateBody ::
      !(Maybe Text),
    _dptrsDescription ::
      !(Maybe Text),
    _dptrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptrsLastModifiedDate' - The date when the fleet provisioning template was last modified.
--
-- * 'dptrsTemplateName' - The name of the fleet provisioning template.
--
-- * 'dptrsPreProvisioningHook' - Gets information about a pre-provisioned hook.
--
-- * 'dptrsEnabled' - True if the fleet provisioning template is enabled, otherwise false.
--
-- * 'dptrsProvisioningRoleARN' - The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- * 'dptrsDefaultVersionId' - The default fleet template version ID.
--
-- * 'dptrsCreationDate' - The date when the fleet provisioning template was created.
--
-- * 'dptrsTemplateARN' - The ARN of the fleet provisioning template.
--
-- * 'dptrsTemplateBody' - The JSON formatted contents of the fleet provisioning template.
--
-- * 'dptrsDescription' - The description of the fleet provisioning template.
--
-- * 'dptrsResponseStatus' - -- | The response status code.
describeProvisioningTemplateResponse ::
  -- | 'dptrsResponseStatus'
  Int ->
  DescribeProvisioningTemplateResponse
describeProvisioningTemplateResponse pResponseStatus_ =
  DescribeProvisioningTemplateResponse'
    { _dptrsLastModifiedDate =
        Nothing,
      _dptrsTemplateName = Nothing,
      _dptrsPreProvisioningHook = Nothing,
      _dptrsEnabled = Nothing,
      _dptrsProvisioningRoleARN = Nothing,
      _dptrsDefaultVersionId = Nothing,
      _dptrsCreationDate = Nothing,
      _dptrsTemplateARN = Nothing,
      _dptrsTemplateBody = Nothing,
      _dptrsDescription = Nothing,
      _dptrsResponseStatus = pResponseStatus_
    }

-- | The date when the fleet provisioning template was last modified.
dptrsLastModifiedDate :: Lens' DescribeProvisioningTemplateResponse (Maybe UTCTime)
dptrsLastModifiedDate = lens _dptrsLastModifiedDate (\s a -> s {_dptrsLastModifiedDate = a}) . mapping _Time

-- | The name of the fleet provisioning template.
dptrsTemplateName :: Lens' DescribeProvisioningTemplateResponse (Maybe Text)
dptrsTemplateName = lens _dptrsTemplateName (\s a -> s {_dptrsTemplateName = a})

-- | Gets information about a pre-provisioned hook.
dptrsPreProvisioningHook :: Lens' DescribeProvisioningTemplateResponse (Maybe ProvisioningHook)
dptrsPreProvisioningHook = lens _dptrsPreProvisioningHook (\s a -> s {_dptrsPreProvisioningHook = a})

-- | True if the fleet provisioning template is enabled, otherwise false.
dptrsEnabled :: Lens' DescribeProvisioningTemplateResponse (Maybe Bool)
dptrsEnabled = lens _dptrsEnabled (\s a -> s {_dptrsEnabled = a})

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
dptrsProvisioningRoleARN :: Lens' DescribeProvisioningTemplateResponse (Maybe Text)
dptrsProvisioningRoleARN = lens _dptrsProvisioningRoleARN (\s a -> s {_dptrsProvisioningRoleARN = a})

-- | The default fleet template version ID.
dptrsDefaultVersionId :: Lens' DescribeProvisioningTemplateResponse (Maybe Int)
dptrsDefaultVersionId = lens _dptrsDefaultVersionId (\s a -> s {_dptrsDefaultVersionId = a})

-- | The date when the fleet provisioning template was created.
dptrsCreationDate :: Lens' DescribeProvisioningTemplateResponse (Maybe UTCTime)
dptrsCreationDate = lens _dptrsCreationDate (\s a -> s {_dptrsCreationDate = a}) . mapping _Time

-- | The ARN of the fleet provisioning template.
dptrsTemplateARN :: Lens' DescribeProvisioningTemplateResponse (Maybe Text)
dptrsTemplateARN = lens _dptrsTemplateARN (\s a -> s {_dptrsTemplateARN = a})

-- | The JSON formatted contents of the fleet provisioning template.
dptrsTemplateBody :: Lens' DescribeProvisioningTemplateResponse (Maybe Text)
dptrsTemplateBody = lens _dptrsTemplateBody (\s a -> s {_dptrsTemplateBody = a})

-- | The description of the fleet provisioning template.
dptrsDescription :: Lens' DescribeProvisioningTemplateResponse (Maybe Text)
dptrsDescription = lens _dptrsDescription (\s a -> s {_dptrsDescription = a})

-- | -- | The response status code.
dptrsResponseStatus :: Lens' DescribeProvisioningTemplateResponse Int
dptrsResponseStatus = lens _dptrsResponseStatus (\s a -> s {_dptrsResponseStatus = a})

instance NFData DescribeProvisioningTemplateResponse
