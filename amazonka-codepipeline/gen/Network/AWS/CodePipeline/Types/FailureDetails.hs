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
-- Module      : Network.AWS.CodePipeline.Types.FailureDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.FailureDetails where

import Network.AWS.CodePipeline.Types.FailureType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about failure details.
--
-- /See:/ 'newFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The external ID of the run of the action that failed.
    externalExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The type of the failure.
    type' :: FailureType,
    -- | The message about the failure.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailureDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalExecutionId', 'failureDetails_externalExecutionId' - The external ID of the run of the action that failed.
--
-- 'type'', 'failureDetails_type' - The type of the failure.
--
-- 'message', 'failureDetails_message' - The message about the failure.
newFailureDetails ::
  -- | 'type''
  FailureType ->
  -- | 'message'
  Prelude.Text ->
  FailureDetails
newFailureDetails pType_ pMessage_ =
  FailureDetails'
    { externalExecutionId =
        Prelude.Nothing,
      type' = pType_,
      message = pMessage_
    }

-- | The external ID of the run of the action that failed.
failureDetails_externalExecutionId :: Lens.Lens' FailureDetails (Prelude.Maybe Prelude.Text)
failureDetails_externalExecutionId = Lens.lens (\FailureDetails' {externalExecutionId} -> externalExecutionId) (\s@FailureDetails' {} a -> s {externalExecutionId = a} :: FailureDetails)

-- | The type of the failure.
failureDetails_type :: Lens.Lens' FailureDetails FailureType
failureDetails_type = Lens.lens (\FailureDetails' {type'} -> type') (\s@FailureDetails' {} a -> s {type' = a} :: FailureDetails)

-- | The message about the failure.
failureDetails_message :: Lens.Lens' FailureDetails Prelude.Text
failureDetails_message = Lens.lens (\FailureDetails' {message} -> message) (\s@FailureDetails' {} a -> s {message = a} :: FailureDetails)

instance Prelude.Hashable FailureDetails

instance Prelude.NFData FailureDetails

instance Prelude.ToJSON FailureDetails where
  toJSON FailureDetails' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("externalExecutionId" Prelude..=)
              Prelude.<$> externalExecutionId,
            Prelude.Just ("type" Prelude..= type'),
            Prelude.Just ("message" Prelude..= message)
          ]
      )
