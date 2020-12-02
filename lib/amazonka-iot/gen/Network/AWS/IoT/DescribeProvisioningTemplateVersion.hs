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
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template version.
module Network.AWS.IoT.DescribeProvisioningTemplateVersion
  ( -- * Creating a Request
    describeProvisioningTemplateVersion,
    DescribeProvisioningTemplateVersion,

    -- * Request Lenses
    dptvTemplateName,
    dptvVersionId,

    -- * Destructuring the Response
    describeProvisioningTemplateVersionResponse,
    DescribeProvisioningTemplateVersionResponse,

    -- * Response Lenses
    dptvrsVersionId,
    dptvrsCreationDate,
    dptvrsTemplateBody,
    dptvrsIsDefaultVersion,
    dptvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeProvisioningTemplateVersion' smart constructor.
data DescribeProvisioningTemplateVersion = DescribeProvisioningTemplateVersion'
  { _dptvTemplateName ::
      !Text,
    _dptvVersionId ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProvisioningTemplateVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptvTemplateName' - The template name.
--
-- * 'dptvVersionId' - The fleet provisioning template version ID.
describeProvisioningTemplateVersion ::
  -- | 'dptvTemplateName'
  Text ->
  -- | 'dptvVersionId'
  Int ->
  DescribeProvisioningTemplateVersion
describeProvisioningTemplateVersion pTemplateName_ pVersionId_ =
  DescribeProvisioningTemplateVersion'
    { _dptvTemplateName =
        pTemplateName_,
      _dptvVersionId = pVersionId_
    }

-- | The template name.
dptvTemplateName :: Lens' DescribeProvisioningTemplateVersion Text
dptvTemplateName = lens _dptvTemplateName (\s a -> s {_dptvTemplateName = a})

-- | The fleet provisioning template version ID.
dptvVersionId :: Lens' DescribeProvisioningTemplateVersion Int
dptvVersionId = lens _dptvVersionId (\s a -> s {_dptvVersionId = a})

instance AWSRequest DescribeProvisioningTemplateVersion where
  type
    Rs DescribeProvisioningTemplateVersion =
      DescribeProvisioningTemplateVersionResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateVersionResponse'
            <$> (x .?> "versionId")
            <*> (x .?> "creationDate")
            <*> (x .?> "templateBody")
            <*> (x .?> "isDefaultVersion")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeProvisioningTemplateVersion

instance NFData DescribeProvisioningTemplateVersion

instance ToHeaders DescribeProvisioningTemplateVersion where
  toHeaders = const mempty

instance ToPath DescribeProvisioningTemplateVersion where
  toPath DescribeProvisioningTemplateVersion' {..} =
    mconcat
      [ "/provisioning-templates/",
        toBS _dptvTemplateName,
        "/versions/",
        toBS _dptvVersionId
      ]

instance ToQuery DescribeProvisioningTemplateVersion where
  toQuery = const mempty

-- | /See:/ 'describeProvisioningTemplateVersionResponse' smart constructor.
data DescribeProvisioningTemplateVersionResponse = DescribeProvisioningTemplateVersionResponse'
  { _dptvrsVersionId ::
      !( Maybe
           Int
       ),
    _dptvrsCreationDate ::
      !( Maybe
           POSIX
       ),
    _dptvrsTemplateBody ::
      !( Maybe
           Text
       ),
    _dptvrsIsDefaultVersion ::
      !( Maybe
           Bool
       ),
    _dptvrsResponseStatus ::
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

-- | Creates a value of 'DescribeProvisioningTemplateVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptvrsVersionId' - The fleet provisioning template version ID.
--
-- * 'dptvrsCreationDate' - The date when the fleet provisioning template version was created.
--
-- * 'dptvrsTemplateBody' - The JSON formatted contents of the fleet provisioning template version.
--
-- * 'dptvrsIsDefaultVersion' - True if the fleet provisioning template version is the default version.
--
-- * 'dptvrsResponseStatus' - -- | The response status code.
describeProvisioningTemplateVersionResponse ::
  -- | 'dptvrsResponseStatus'
  Int ->
  DescribeProvisioningTemplateVersionResponse
describeProvisioningTemplateVersionResponse pResponseStatus_ =
  DescribeProvisioningTemplateVersionResponse'
    { _dptvrsVersionId =
        Nothing,
      _dptvrsCreationDate = Nothing,
      _dptvrsTemplateBody = Nothing,
      _dptvrsIsDefaultVersion = Nothing,
      _dptvrsResponseStatus = pResponseStatus_
    }

-- | The fleet provisioning template version ID.
dptvrsVersionId :: Lens' DescribeProvisioningTemplateVersionResponse (Maybe Int)
dptvrsVersionId = lens _dptvrsVersionId (\s a -> s {_dptvrsVersionId = a})

-- | The date when the fleet provisioning template version was created.
dptvrsCreationDate :: Lens' DescribeProvisioningTemplateVersionResponse (Maybe UTCTime)
dptvrsCreationDate = lens _dptvrsCreationDate (\s a -> s {_dptvrsCreationDate = a}) . mapping _Time

-- | The JSON formatted contents of the fleet provisioning template version.
dptvrsTemplateBody :: Lens' DescribeProvisioningTemplateVersionResponse (Maybe Text)
dptvrsTemplateBody = lens _dptvrsTemplateBody (\s a -> s {_dptvrsTemplateBody = a})

-- | True if the fleet provisioning template version is the default version.
dptvrsIsDefaultVersion :: Lens' DescribeProvisioningTemplateVersionResponse (Maybe Bool)
dptvrsIsDefaultVersion = lens _dptvrsIsDefaultVersion (\s a -> s {_dptvrsIsDefaultVersion = a})

-- | -- | The response status code.
dptvrsResponseStatus :: Lens' DescribeProvisioningTemplateVersionResponse Int
dptvrsResponseStatus = lens _dptvrsResponseStatus (\s a -> s {_dptvrsResponseStatus = a})

instance NFData DescribeProvisioningTemplateVersionResponse
