{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a request to compile the specified type of information of the
-- deployed environment.
--
-- Setting the @InfoType@ to @tail@ compiles the last lines from the
-- application server log files of every Amazon EC2 instance in your
-- environment.
--
-- Setting the @InfoType@ to @bundle@ compresses the application server log
-- files for every Amazon EC2 instance into a @.zip@ file. Legacy and .NET
-- containers do not support bundle logs.
--
-- Use RetrieveEnvironmentInfo to obtain the set of logs.
--
-- Related Topics
--
-- -   RetrieveEnvironmentInfo
module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
  ( -- * Creating a Request
    RequestEnvironmentInfo (..),
    newRequestEnvironmentInfo,

    -- * Request Lenses
    requestEnvironmentInfo_environmentId,
    requestEnvironmentInfo_environmentName,
    requestEnvironmentInfo_infoType,

    -- * Destructuring the Response
    RequestEnvironmentInfoResponse (..),
    newRequestEnvironmentInfoResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to retrieve logs from an environment and store them in your
-- Elastic Beanstalk storage bucket.
--
-- /See:/ 'newRequestEnvironmentInfo' smart constructor.
data RequestEnvironmentInfo = RequestEnvironmentInfo'
  { -- | The ID of the environment of the requested data.
    --
    -- If no such environment is found, @RequestEnvironmentInfo@ returns an
    -- @InvalidParameterValue@ error.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the environment of the requested data.
    --
    -- If no such environment is found, @RequestEnvironmentInfo@ returns an
    -- @InvalidParameterValue@ error.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The type of information to request.
    infoType :: EnvironmentInfoType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RequestEnvironmentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'requestEnvironmentInfo_environmentId' - The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'environmentName', 'requestEnvironmentInfo_environmentName' - The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'infoType', 'requestEnvironmentInfo_infoType' - The type of information to request.
newRequestEnvironmentInfo ::
  -- | 'infoType'
  EnvironmentInfoType ->
  RequestEnvironmentInfo
newRequestEnvironmentInfo pInfoType_ =
  RequestEnvironmentInfo'
    { environmentId =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      infoType = pInfoType_
    }

-- | The ID of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
requestEnvironmentInfo_environmentId :: Lens.Lens' RequestEnvironmentInfo (Prelude.Maybe Prelude.Text)
requestEnvironmentInfo_environmentId = Lens.lens (\RequestEnvironmentInfo' {environmentId} -> environmentId) (\s@RequestEnvironmentInfo' {} a -> s {environmentId = a} :: RequestEnvironmentInfo)

-- | The name of the environment of the requested data.
--
-- If no such environment is found, @RequestEnvironmentInfo@ returns an
-- @InvalidParameterValue@ error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
requestEnvironmentInfo_environmentName :: Lens.Lens' RequestEnvironmentInfo (Prelude.Maybe Prelude.Text)
requestEnvironmentInfo_environmentName = Lens.lens (\RequestEnvironmentInfo' {environmentName} -> environmentName) (\s@RequestEnvironmentInfo' {} a -> s {environmentName = a} :: RequestEnvironmentInfo)

-- | The type of information to request.
requestEnvironmentInfo_infoType :: Lens.Lens' RequestEnvironmentInfo EnvironmentInfoType
requestEnvironmentInfo_infoType = Lens.lens (\RequestEnvironmentInfo' {infoType} -> infoType) (\s@RequestEnvironmentInfo' {} a -> s {infoType = a} :: RequestEnvironmentInfo)

instance Prelude.AWSRequest RequestEnvironmentInfo where
  type
    Rs RequestEnvironmentInfo =
      RequestEnvironmentInfoResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      RequestEnvironmentInfoResponse'

instance Prelude.Hashable RequestEnvironmentInfo

instance Prelude.NFData RequestEnvironmentInfo

instance Prelude.ToHeaders RequestEnvironmentInfo where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RequestEnvironmentInfo where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RequestEnvironmentInfo where
  toQuery RequestEnvironmentInfo' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RequestEnvironmentInfo" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentId" Prelude.=: environmentId,
        "EnvironmentName" Prelude.=: environmentName,
        "InfoType" Prelude.=: infoType
      ]

-- | /See:/ 'newRequestEnvironmentInfoResponse' smart constructor.
data RequestEnvironmentInfoResponse = RequestEnvironmentInfoResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RequestEnvironmentInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRequestEnvironmentInfoResponse ::
  RequestEnvironmentInfoResponse
newRequestEnvironmentInfoResponse =
  RequestEnvironmentInfoResponse'

instance
  Prelude.NFData
    RequestEnvironmentInfoResponse
