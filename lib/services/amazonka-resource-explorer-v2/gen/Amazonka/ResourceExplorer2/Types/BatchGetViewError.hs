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
-- Module      : Amazonka.ResourceExplorer2.Types.BatchGetViewError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Types.BatchGetViewError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A collection of error messages for any views that Amazon Web Services
-- Resource Explorer couldn\'t retrieve details.
--
-- /See:/ 'newBatchGetViewError' smart constructor.
data BatchGetViewError = BatchGetViewError'
  { -- | The description of the error for the specified view.
    errorMessage :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the view for which Resource Explorer failed to retrieve details.
    viewArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetViewError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchGetViewError_errorMessage' - The description of the error for the specified view.
--
-- 'viewArn', 'batchGetViewError_viewArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view for which Resource Explorer failed to retrieve details.
newBatchGetViewError ::
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'viewArn'
  Prelude.Text ->
  BatchGetViewError
newBatchGetViewError pErrorMessage_ pViewArn_ =
  BatchGetViewError'
    { errorMessage = pErrorMessage_,
      viewArn = pViewArn_
    }

-- | The description of the error for the specified view.
batchGetViewError_errorMessage :: Lens.Lens' BatchGetViewError Prelude.Text
batchGetViewError_errorMessage = Lens.lens (\BatchGetViewError' {errorMessage} -> errorMessage) (\s@BatchGetViewError' {} a -> s {errorMessage = a} :: BatchGetViewError)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the view for which Resource Explorer failed to retrieve details.
batchGetViewError_viewArn :: Lens.Lens' BatchGetViewError Prelude.Text
batchGetViewError_viewArn = Lens.lens (\BatchGetViewError' {viewArn} -> viewArn) (\s@BatchGetViewError' {} a -> s {viewArn = a} :: BatchGetViewError)

instance Core.FromJSON BatchGetViewError where
  parseJSON =
    Core.withObject
      "BatchGetViewError"
      ( \x ->
          BatchGetViewError'
            Prelude.<$> (x Core..: "ErrorMessage")
            Prelude.<*> (x Core..: "ViewArn")
      )

instance Prelude.Hashable BatchGetViewError where
  hashWithSalt _salt BatchGetViewError' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` viewArn

instance Prelude.NFData BatchGetViewError where
  rnf BatchGetViewError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf viewArn
