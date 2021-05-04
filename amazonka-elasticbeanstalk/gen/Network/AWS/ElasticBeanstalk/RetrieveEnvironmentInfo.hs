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
-- Module      : Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the compiled information from a RequestEnvironmentInfo
-- request.
--
-- Related Topics
--
-- -   RequestEnvironmentInfo
module Network.AWS.ElasticBeanstalk.RetrieveEnvironmentInfo
  ( -- * Creating a Request
    RetrieveEnvironmentInfo (..),
    newRetrieveEnvironmentInfo,

    -- * Request Lenses
    retrieveEnvironmentInfo_environmentId,
    retrieveEnvironmentInfo_environmentName,
    retrieveEnvironmentInfo_infoType,

    -- * Destructuring the Response
    RetrieveEnvironmentInfoResponse (..),
    newRetrieveEnvironmentInfoResponse,

    -- * Response Lenses
    retrieveEnvironmentInfoResponse_environmentInfo,
    retrieveEnvironmentInfoResponse_httpStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to download logs retrieved with RequestEnvironmentInfo.
--
-- /See:/ 'newRetrieveEnvironmentInfo' smart constructor.
data RetrieveEnvironmentInfo = RetrieveEnvironmentInfo'
  { -- | The ID of the data\'s environment.
    --
    -- If no such environment is found, returns an @InvalidParameterValue@
    -- error.
    --
    -- Condition: You must specify either this or an EnvironmentName, or both.
    -- If you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the data\'s environment.
    --
    -- If no such environment is found, returns an @InvalidParameterValue@
    -- error.
    --
    -- Condition: You must specify either this or an EnvironmentId, or both. If
    -- you do not specify either, AWS Elastic Beanstalk returns
    -- @MissingRequiredParameter@ error.
    environmentName :: Prelude.Maybe Prelude.Text,
    -- | The type of information to retrieve.
    infoType :: EnvironmentInfoType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RetrieveEnvironmentInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'retrieveEnvironmentInfo_environmentId' - The ID of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'environmentName', 'retrieveEnvironmentInfo_environmentName' - The name of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
--
-- 'infoType', 'retrieveEnvironmentInfo_infoType' - The type of information to retrieve.
newRetrieveEnvironmentInfo ::
  -- | 'infoType'
  EnvironmentInfoType ->
  RetrieveEnvironmentInfo
newRetrieveEnvironmentInfo pInfoType_ =
  RetrieveEnvironmentInfo'
    { environmentId =
        Prelude.Nothing,
      environmentName = Prelude.Nothing,
      infoType = pInfoType_
    }

-- | The ID of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentName, or both.
-- If you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
retrieveEnvironmentInfo_environmentId :: Lens.Lens' RetrieveEnvironmentInfo (Prelude.Maybe Prelude.Text)
retrieveEnvironmentInfo_environmentId = Lens.lens (\RetrieveEnvironmentInfo' {environmentId} -> environmentId) (\s@RetrieveEnvironmentInfo' {} a -> s {environmentId = a} :: RetrieveEnvironmentInfo)

-- | The name of the data\'s environment.
--
-- If no such environment is found, returns an @InvalidParameterValue@
-- error.
--
-- Condition: You must specify either this or an EnvironmentId, or both. If
-- you do not specify either, AWS Elastic Beanstalk returns
-- @MissingRequiredParameter@ error.
retrieveEnvironmentInfo_environmentName :: Lens.Lens' RetrieveEnvironmentInfo (Prelude.Maybe Prelude.Text)
retrieveEnvironmentInfo_environmentName = Lens.lens (\RetrieveEnvironmentInfo' {environmentName} -> environmentName) (\s@RetrieveEnvironmentInfo' {} a -> s {environmentName = a} :: RetrieveEnvironmentInfo)

-- | The type of information to retrieve.
retrieveEnvironmentInfo_infoType :: Lens.Lens' RetrieveEnvironmentInfo EnvironmentInfoType
retrieveEnvironmentInfo_infoType = Lens.lens (\RetrieveEnvironmentInfo' {infoType} -> infoType) (\s@RetrieveEnvironmentInfo' {} a -> s {infoType = a} :: RetrieveEnvironmentInfo)

instance Prelude.AWSRequest RetrieveEnvironmentInfo where
  type
    Rs RetrieveEnvironmentInfo =
      RetrieveEnvironmentInfoResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RetrieveEnvironmentInfoResult"
      ( \s h x ->
          RetrieveEnvironmentInfoResponse'
            Prelude.<$> ( x Prelude..@? "EnvironmentInfo"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RetrieveEnvironmentInfo

instance Prelude.NFData RetrieveEnvironmentInfo

instance Prelude.ToHeaders RetrieveEnvironmentInfo where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RetrieveEnvironmentInfo where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RetrieveEnvironmentInfo where
  toQuery RetrieveEnvironmentInfo' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RetrieveEnvironmentInfo" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentId" Prelude.=: environmentId,
        "EnvironmentName" Prelude.=: environmentName,
        "InfoType" Prelude.=: infoType
      ]

-- | Result message containing a description of the requested environment
-- info.
--
-- /See:/ 'newRetrieveEnvironmentInfoResponse' smart constructor.
data RetrieveEnvironmentInfoResponse = RetrieveEnvironmentInfoResponse'
  { -- | The EnvironmentInfoDescription of the environment.
    environmentInfo :: Prelude.Maybe [EnvironmentInfoDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RetrieveEnvironmentInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentInfo', 'retrieveEnvironmentInfoResponse_environmentInfo' - The EnvironmentInfoDescription of the environment.
--
-- 'httpStatus', 'retrieveEnvironmentInfoResponse_httpStatus' - The response's http status code.
newRetrieveEnvironmentInfoResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RetrieveEnvironmentInfoResponse
newRetrieveEnvironmentInfoResponse pHttpStatus_ =
  RetrieveEnvironmentInfoResponse'
    { environmentInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The EnvironmentInfoDescription of the environment.
retrieveEnvironmentInfoResponse_environmentInfo :: Lens.Lens' RetrieveEnvironmentInfoResponse (Prelude.Maybe [EnvironmentInfoDescription])
retrieveEnvironmentInfoResponse_environmentInfo = Lens.lens (\RetrieveEnvironmentInfoResponse' {environmentInfo} -> environmentInfo) (\s@RetrieveEnvironmentInfoResponse' {} a -> s {environmentInfo = a} :: RetrieveEnvironmentInfoResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
retrieveEnvironmentInfoResponse_httpStatus :: Lens.Lens' RetrieveEnvironmentInfoResponse Prelude.Int
retrieveEnvironmentInfoResponse_httpStatus = Lens.lens (\RetrieveEnvironmentInfoResponse' {httpStatus} -> httpStatus) (\s@RetrieveEnvironmentInfoResponse' {} a -> s {httpStatus = a} :: RetrieveEnvironmentInfoResponse)

instance
  Prelude.NFData
    RetrieveEnvironmentInfoResponse
