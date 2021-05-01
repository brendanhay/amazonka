{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It
-- isn\'t set for other event types.
--
-- /See:/ 'newScheduleLambdaFunctionFailedEventAttributes' smart constructor.
data ScheduleLambdaFunctionFailedEventAttributes = ScheduleLambdaFunctionFailedEventAttributes'
  { -- | The ID provided in the @ScheduleLambdaFunction@ decision that failed.
    id :: Prelude.Text,
    -- | The name of the Lambda function.
    name :: Prelude.Text,
    -- | The cause of the failure. To help diagnose issues, use this information
    -- to trace back the chain of events leading up to this event.
    --
    -- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
    -- because it lacked sufficient permissions. For details and example IAM
    -- policies, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
    -- in the /Amazon SWF Developer Guide/.
    cause :: ScheduleLambdaFunctionFailedCause,
    -- | The ID of the @LambdaFunctionCompleted@ event corresponding to the
    -- decision that resulted in scheduling this Lambda task. To help diagnose
    -- issues, use this information to trace back the chain of events leading
    -- up to this event.
    decisionTaskCompletedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScheduleLambdaFunctionFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'scheduleLambdaFunctionFailedEventAttributes_id' - The ID provided in the @ScheduleLambdaFunction@ decision that failed.
--
-- 'name', 'scheduleLambdaFunctionFailedEventAttributes_name' - The name of the Lambda function.
--
-- 'cause', 'scheduleLambdaFunctionFailedEventAttributes_cause' - The cause of the failure. To help diagnose issues, use this information
-- to trace back the chain of events leading up to this event.
--
-- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- 'decisionTaskCompletedEventId', 'scheduleLambdaFunctionFailedEventAttributes_decisionTaskCompletedEventId' - The ID of the @LambdaFunctionCompleted@ event corresponding to the
-- decision that resulted in scheduling this Lambda task. To help diagnose
-- issues, use this information to trace back the chain of events leading
-- up to this event.
newScheduleLambdaFunctionFailedEventAttributes ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'cause'
  ScheduleLambdaFunctionFailedCause ->
  -- | 'decisionTaskCompletedEventId'
  Prelude.Integer ->
  ScheduleLambdaFunctionFailedEventAttributes
newScheduleLambdaFunctionFailedEventAttributes
  pId_
  pName_
  pCause_
  pDecisionTaskCompletedEventId_ =
    ScheduleLambdaFunctionFailedEventAttributes'
      { id =
          pId_,
        name = pName_,
        cause = pCause_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The ID provided in the @ScheduleLambdaFunction@ decision that failed.
scheduleLambdaFunctionFailedEventAttributes_id :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Prelude.Text
scheduleLambdaFunctionFailedEventAttributes_id = Lens.lens (\ScheduleLambdaFunctionFailedEventAttributes' {id} -> id) (\s@ScheduleLambdaFunctionFailedEventAttributes' {} a -> s {id = a} :: ScheduleLambdaFunctionFailedEventAttributes)

-- | The name of the Lambda function.
scheduleLambdaFunctionFailedEventAttributes_name :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Prelude.Text
scheduleLambdaFunctionFailedEventAttributes_name = Lens.lens (\ScheduleLambdaFunctionFailedEventAttributes' {name} -> name) (\s@ScheduleLambdaFunctionFailedEventAttributes' {} a -> s {name = a} :: ScheduleLambdaFunctionFailedEventAttributes)

-- | The cause of the failure. To help diagnose issues, use this information
-- to trace back the chain of events leading up to this event.
--
-- If @cause@ is set to @OPERATION_NOT_PERMITTED@, the decision failed
-- because it lacked sufficient permissions. For details and example IAM
-- policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
scheduleLambdaFunctionFailedEventAttributes_cause :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes ScheduleLambdaFunctionFailedCause
scheduleLambdaFunctionFailedEventAttributes_cause = Lens.lens (\ScheduleLambdaFunctionFailedEventAttributes' {cause} -> cause) (\s@ScheduleLambdaFunctionFailedEventAttributes' {} a -> s {cause = a} :: ScheduleLambdaFunctionFailedEventAttributes)

-- | The ID of the @LambdaFunctionCompleted@ event corresponding to the
-- decision that resulted in scheduling this Lambda task. To help diagnose
-- issues, use this information to trace back the chain of events leading
-- up to this event.
scheduleLambdaFunctionFailedEventAttributes_decisionTaskCompletedEventId :: Lens.Lens' ScheduleLambdaFunctionFailedEventAttributes Prelude.Integer
scheduleLambdaFunctionFailedEventAttributes_decisionTaskCompletedEventId = Lens.lens (\ScheduleLambdaFunctionFailedEventAttributes' {decisionTaskCompletedEventId} -> decisionTaskCompletedEventId) (\s@ScheduleLambdaFunctionFailedEventAttributes' {} a -> s {decisionTaskCompletedEventId = a} :: ScheduleLambdaFunctionFailedEventAttributes)

instance
  Prelude.FromJSON
    ScheduleLambdaFunctionFailedEventAttributes
  where
  parseJSON =
    Prelude.withObject
      "ScheduleLambdaFunctionFailedEventAttributes"
      ( \x ->
          ScheduleLambdaFunctionFailedEventAttributes'
            Prelude.<$> (x Prelude..: "id") Prelude.<*> (x Prelude..: "name")
              Prelude.<*> (x Prelude..: "cause")
              Prelude.<*> (x Prelude..: "decisionTaskCompletedEventId")
      )

instance
  Prelude.Hashable
    ScheduleLambdaFunctionFailedEventAttributes

instance
  Prelude.NFData
    ScheduleLambdaFunctionFailedEventAttributes
