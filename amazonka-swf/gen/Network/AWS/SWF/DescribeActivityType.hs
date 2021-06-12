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
-- Module      : Network.AWS.SWF.DescribeActivityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified activity type. This includes
-- configuration settings provided when the type was registered and other
-- general information about the type.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @activityType.name@: String constraint. The key is
--         @swf:activityType.name@.
--
--     -   @activityType.version@: String constraint. The key is
--         @swf:activityType.version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Network.AWS.SWF.DescribeActivityType
  ( -- * Creating a Request
    DescribeActivityType (..),
    newDescribeActivityType,

    -- * Request Lenses
    describeActivityType_domain,
    describeActivityType_activityType,

    -- * Destructuring the Response
    DescribeActivityTypeResponse (..),
    newDescribeActivityTypeResponse,

    -- * Response Lenses
    describeActivityTypeResponse_httpStatus,
    describeActivityTypeResponse_typeInfo,
    describeActivityTypeResponse_configuration,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newDescribeActivityType' smart constructor.
data DescribeActivityType = DescribeActivityType'
  { -- | The name of the domain in which the activity type is registered.
    domain :: Core.Text,
    -- | The activity type to get information about. Activity types are
    -- identified by the @name@ and @version@ that were supplied when the
    -- activity was registered.
    activityType :: ActivityType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeActivityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'describeActivityType_domain' - The name of the domain in which the activity type is registered.
--
-- 'activityType', 'describeActivityType_activityType' - The activity type to get information about. Activity types are
-- identified by the @name@ and @version@ that were supplied when the
-- activity was registered.
newDescribeActivityType ::
  -- | 'domain'
  Core.Text ->
  -- | 'activityType'
  ActivityType ->
  DescribeActivityType
newDescribeActivityType pDomain_ pActivityType_ =
  DescribeActivityType'
    { domain = pDomain_,
      activityType = pActivityType_
    }

-- | The name of the domain in which the activity type is registered.
describeActivityType_domain :: Lens.Lens' DescribeActivityType Core.Text
describeActivityType_domain = Lens.lens (\DescribeActivityType' {domain} -> domain) (\s@DescribeActivityType' {} a -> s {domain = a} :: DescribeActivityType)

-- | The activity type to get information about. Activity types are
-- identified by the @name@ and @version@ that were supplied when the
-- activity was registered.
describeActivityType_activityType :: Lens.Lens' DescribeActivityType ActivityType
describeActivityType_activityType = Lens.lens (\DescribeActivityType' {activityType} -> activityType) (\s@DescribeActivityType' {} a -> s {activityType = a} :: DescribeActivityType)

instance Core.AWSRequest DescribeActivityType where
  type
    AWSResponse DescribeActivityType =
      DescribeActivityTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeActivityTypeResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "typeInfo")
            Core.<*> (x Core..:> "configuration")
      )

instance Core.Hashable DescribeActivityType

instance Core.NFData DescribeActivityType

instance Core.ToHeaders DescribeActivityType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.DescribeActivityType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeActivityType where
  toJSON DescribeActivityType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("domain" Core..= domain),
            Core.Just ("activityType" Core..= activityType)
          ]
      )

instance Core.ToPath DescribeActivityType where
  toPath = Core.const "/"

instance Core.ToQuery DescribeActivityType where
  toQuery = Core.const Core.mempty

-- | Detailed information about an activity type.
--
-- /See:/ 'newDescribeActivityTypeResponse' smart constructor.
data DescribeActivityTypeResponse = DescribeActivityTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | General information about the activity type.
    --
    -- The status of activity type (returned in the ActivityTypeInfo structure)
    -- can be one of the following.
    --
    -- -   @REGISTERED@ – The type is registered and available. Workers
    --     supporting this type should be running.
    --
    -- -   @DEPRECATED@ – The type was deprecated using DeprecateActivityType,
    --     but is still in use. You should keep workers supporting this type
    --     running. You cannot create new tasks of this type.
    typeInfo :: ActivityTypeInfo,
    -- | The configuration settings registered with the activity type.
    configuration :: ActivityTypeConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeActivityTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeActivityTypeResponse_httpStatus' - The response's http status code.
--
-- 'typeInfo', 'describeActivityTypeResponse_typeInfo' - General information about the activity type.
--
-- The status of activity type (returned in the ActivityTypeInfo structure)
-- can be one of the following.
--
-- -   @REGISTERED@ – The type is registered and available. Workers
--     supporting this type should be running.
--
-- -   @DEPRECATED@ – The type was deprecated using DeprecateActivityType,
--     but is still in use. You should keep workers supporting this type
--     running. You cannot create new tasks of this type.
--
-- 'configuration', 'describeActivityTypeResponse_configuration' - The configuration settings registered with the activity type.
newDescribeActivityTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'typeInfo'
  ActivityTypeInfo ->
  -- | 'configuration'
  ActivityTypeConfiguration ->
  DescribeActivityTypeResponse
newDescribeActivityTypeResponse
  pHttpStatus_
  pTypeInfo_
  pConfiguration_ =
    DescribeActivityTypeResponse'
      { httpStatus =
          pHttpStatus_,
        typeInfo = pTypeInfo_,
        configuration = pConfiguration_
      }

-- | The response's http status code.
describeActivityTypeResponse_httpStatus :: Lens.Lens' DescribeActivityTypeResponse Core.Int
describeActivityTypeResponse_httpStatus = Lens.lens (\DescribeActivityTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeActivityTypeResponse' {} a -> s {httpStatus = a} :: DescribeActivityTypeResponse)

-- | General information about the activity type.
--
-- The status of activity type (returned in the ActivityTypeInfo structure)
-- can be one of the following.
--
-- -   @REGISTERED@ – The type is registered and available. Workers
--     supporting this type should be running.
--
-- -   @DEPRECATED@ – The type was deprecated using DeprecateActivityType,
--     but is still in use. You should keep workers supporting this type
--     running. You cannot create new tasks of this type.
describeActivityTypeResponse_typeInfo :: Lens.Lens' DescribeActivityTypeResponse ActivityTypeInfo
describeActivityTypeResponse_typeInfo = Lens.lens (\DescribeActivityTypeResponse' {typeInfo} -> typeInfo) (\s@DescribeActivityTypeResponse' {} a -> s {typeInfo = a} :: DescribeActivityTypeResponse)

-- | The configuration settings registered with the activity type.
describeActivityTypeResponse_configuration :: Lens.Lens' DescribeActivityTypeResponse ActivityTypeConfiguration
describeActivityTypeResponse_configuration = Lens.lens (\DescribeActivityTypeResponse' {configuration} -> configuration) (\s@DescribeActivityTypeResponse' {} a -> s {configuration = a} :: DescribeActivityTypeResponse)

instance Core.NFData DescribeActivityTypeResponse
