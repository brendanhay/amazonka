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
-- Module      : Amazonka.SecurityHub.Types.StandardsSubscriptionRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsSubscriptionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The standard that you want to enable.
--
-- /See:/ 'newStandardsSubscriptionRequest' smart constructor.
data StandardsSubscriptionRequest = StandardsSubscriptionRequest'
  { -- | A key-value pair of input for the standard.
    standardsInput :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the standard that you want to enable. To view the list of
    -- available standards and their ARNs, use the @DescribeStandards@
    -- operation.
    standardsArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsSubscriptionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsInput', 'standardsSubscriptionRequest_standardsInput' - A key-value pair of input for the standard.
--
-- 'standardsArn', 'standardsSubscriptionRequest_standardsArn' - The ARN of the standard that you want to enable. To view the list of
-- available standards and their ARNs, use the @DescribeStandards@
-- operation.
newStandardsSubscriptionRequest ::
  -- | 'standardsArn'
  Prelude.Text ->
  StandardsSubscriptionRequest
newStandardsSubscriptionRequest pStandardsArn_ =
  StandardsSubscriptionRequest'
    { standardsInput =
        Prelude.Nothing,
      standardsArn = pStandardsArn_
    }

-- | A key-value pair of input for the standard.
standardsSubscriptionRequest_standardsInput :: Lens.Lens' StandardsSubscriptionRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
standardsSubscriptionRequest_standardsInput = Lens.lens (\StandardsSubscriptionRequest' {standardsInput} -> standardsInput) (\s@StandardsSubscriptionRequest' {} a -> s {standardsInput = a} :: StandardsSubscriptionRequest) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the standard that you want to enable. To view the list of
-- available standards and their ARNs, use the @DescribeStandards@
-- operation.
standardsSubscriptionRequest_standardsArn :: Lens.Lens' StandardsSubscriptionRequest Prelude.Text
standardsSubscriptionRequest_standardsArn = Lens.lens (\StandardsSubscriptionRequest' {standardsArn} -> standardsArn) (\s@StandardsSubscriptionRequest' {} a -> s {standardsArn = a} :: StandardsSubscriptionRequest)

instance
  Prelude.Hashable
    StandardsSubscriptionRequest
  where
  hashWithSalt _salt StandardsSubscriptionRequest' {..} =
    _salt
      `Prelude.hashWithSalt` standardsInput
      `Prelude.hashWithSalt` standardsArn

instance Prelude.NFData StandardsSubscriptionRequest where
  rnf StandardsSubscriptionRequest' {..} =
    Prelude.rnf standardsInput `Prelude.seq`
      Prelude.rnf standardsArn

instance Data.ToJSON StandardsSubscriptionRequest where
  toJSON StandardsSubscriptionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StandardsInput" Data..=)
              Prelude.<$> standardsInput,
            Prelude.Just ("StandardsArn" Data..= standardsArn)
          ]
      )
