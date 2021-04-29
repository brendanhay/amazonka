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
-- Module      : Network.AWS.SWF.DeprecateActivityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates the specified /activity type/. After an activity type has
-- been deprecated, you cannot create new tasks of that activity type.
-- Tasks of this type that were scheduled before the type was deprecated
-- continue to run.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
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
module Network.AWS.SWF.DeprecateActivityType
  ( -- * Creating a Request
    DeprecateActivityType (..),
    newDeprecateActivityType,

    -- * Request Lenses
    deprecateActivityType_domain,
    deprecateActivityType_activityType,

    -- * Destructuring the Response
    DeprecateActivityTypeResponse (..),
    newDeprecateActivityTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newDeprecateActivityType' smart constructor.
data DeprecateActivityType = DeprecateActivityType'
  { -- | The name of the domain in which the activity type is registered.
    domain :: Prelude.Text,
    -- | The activity type to deprecate.
    activityType :: ActivityType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeprecateActivityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'deprecateActivityType_domain' - The name of the domain in which the activity type is registered.
--
-- 'activityType', 'deprecateActivityType_activityType' - The activity type to deprecate.
newDeprecateActivityType ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'activityType'
  ActivityType ->
  DeprecateActivityType
newDeprecateActivityType pDomain_ pActivityType_ =
  DeprecateActivityType'
    { domain = pDomain_,
      activityType = pActivityType_
    }

-- | The name of the domain in which the activity type is registered.
deprecateActivityType_domain :: Lens.Lens' DeprecateActivityType Prelude.Text
deprecateActivityType_domain = Lens.lens (\DeprecateActivityType' {domain} -> domain) (\s@DeprecateActivityType' {} a -> s {domain = a} :: DeprecateActivityType)

-- | The activity type to deprecate.
deprecateActivityType_activityType :: Lens.Lens' DeprecateActivityType ActivityType
deprecateActivityType_activityType = Lens.lens (\DeprecateActivityType' {activityType} -> activityType) (\s@DeprecateActivityType' {} a -> s {activityType = a} :: DeprecateActivityType)

instance Prelude.AWSRequest DeprecateActivityType where
  type
    Rs DeprecateActivityType =
      DeprecateActivityTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeprecateActivityTypeResponse'

instance Prelude.Hashable DeprecateActivityType

instance Prelude.NFData DeprecateActivityType

instance Prelude.ToHeaders DeprecateActivityType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.DeprecateActivityType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeprecateActivityType where
  toJSON DeprecateActivityType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("domain" Prelude..= domain),
            Prelude.Just
              ("activityType" Prelude..= activityType)
          ]
      )

instance Prelude.ToPath DeprecateActivityType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeprecateActivityType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeprecateActivityTypeResponse' smart constructor.
data DeprecateActivityTypeResponse = DeprecateActivityTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeprecateActivityTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeprecateActivityTypeResponse ::
  DeprecateActivityTypeResponse
newDeprecateActivityTypeResponse =
  DeprecateActivityTypeResponse'

instance Prelude.NFData DeprecateActivityTypeResponse
