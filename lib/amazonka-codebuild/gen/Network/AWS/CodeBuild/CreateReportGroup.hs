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
-- Module      : Network.AWS.CodeBuild.CreateReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a report group. A report group contains a collection of reports.
module Network.AWS.CodeBuild.CreateReportGroup
  ( -- * Creating a Request
    createReportGroup,
    CreateReportGroup,

    -- * Request Lenses
    crgTags,
    crgName,
    crgType,
    crgExportConfig,

    -- * Destructuring the Response
    createReportGroupResponse,
    CreateReportGroupResponse,

    -- * Response Lenses
    crgrsReportGroup,
    crgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createReportGroup' smart constructor.
data CreateReportGroup = CreateReportGroup'
  { _crgTags ::
      !(Maybe [Tag]),
    _crgName :: !Text,
    _crgType :: !ReportType,
    _crgExportConfig :: !ReportExportConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReportGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgTags' - A list of tag key and value pairs associated with this report group.  These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- * 'crgName' - The name of the report group.
--
-- * 'crgType' - The type of report group.
--
-- * 'crgExportConfig' - A @ReportExportConfig@ object that contains information about where the report group test results are exported.
createReportGroup ::
  -- | 'crgName'
  Text ->
  -- | 'crgType'
  ReportType ->
  -- | 'crgExportConfig'
  ReportExportConfig ->
  CreateReportGroup
createReportGroup pName_ pType_ pExportConfig_ =
  CreateReportGroup'
    { _crgTags = Nothing,
      _crgName = pName_,
      _crgType = pType_,
      _crgExportConfig = pExportConfig_
    }

-- | A list of tag key and value pairs associated with this report group.  These tags are available for use by AWS services that support AWS CodeBuild report group tags.
crgTags :: Lens' CreateReportGroup [Tag]
crgTags = lens _crgTags (\s a -> s {_crgTags = a}) . _Default . _Coerce

-- | The name of the report group.
crgName :: Lens' CreateReportGroup Text
crgName = lens _crgName (\s a -> s {_crgName = a})

-- | The type of report group.
crgType :: Lens' CreateReportGroup ReportType
crgType = lens _crgType (\s a -> s {_crgType = a})

-- | A @ReportExportConfig@ object that contains information about where the report group test results are exported.
crgExportConfig :: Lens' CreateReportGroup ReportExportConfig
crgExportConfig = lens _crgExportConfig (\s a -> s {_crgExportConfig = a})

instance AWSRequest CreateReportGroup where
  type Rs CreateReportGroup = CreateReportGroupResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          CreateReportGroupResponse'
            <$> (x .?> "reportGroup") <*> (pure (fromEnum s))
      )

instance Hashable CreateReportGroup

instance NFData CreateReportGroup

instance ToHeaders CreateReportGroup where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.CreateReportGroup" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateReportGroup where
  toJSON CreateReportGroup' {..} =
    object
      ( catMaybes
          [ ("tags" .=) <$> _crgTags,
            Just ("name" .= _crgName),
            Just ("type" .= _crgType),
            Just ("exportConfig" .= _crgExportConfig)
          ]
      )

instance ToPath CreateReportGroup where
  toPath = const "/"

instance ToQuery CreateReportGroup where
  toQuery = const mempty

-- | /See:/ 'createReportGroupResponse' smart constructor.
data CreateReportGroupResponse = CreateReportGroupResponse'
  { _crgrsReportGroup ::
      !(Maybe ReportGroup),
    _crgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateReportGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgrsReportGroup' - Information about the report group that was created.
--
-- * 'crgrsResponseStatus' - -- | The response status code.
createReportGroupResponse ::
  -- | 'crgrsResponseStatus'
  Int ->
  CreateReportGroupResponse
createReportGroupResponse pResponseStatus_ =
  CreateReportGroupResponse'
    { _crgrsReportGroup = Nothing,
      _crgrsResponseStatus = pResponseStatus_
    }

-- | Information about the report group that was created.
crgrsReportGroup :: Lens' CreateReportGroupResponse (Maybe ReportGroup)
crgrsReportGroup = lens _crgrsReportGroup (\s a -> s {_crgrsReportGroup = a})

-- | -- | The response status code.
crgrsResponseStatus :: Lens' CreateReportGroupResponse Int
crgrsResponseStatus = lens _crgrsResponseStatus (\s a -> s {_crgrsResponseStatus = a})

instance NFData CreateReportGroupResponse
