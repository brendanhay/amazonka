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
-- Module      : Amazonka.SecurityHub.Types.StandardsSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.StandardsSubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.StandardsStatus
import Amazonka.SecurityHub.Types.StandardsStatusReason

-- | A resource that represents your subscription to a supported standard.
--
-- /See:/ 'newStandardsSubscription' smart constructor.
data StandardsSubscription = StandardsSubscription'
  { -- | The reason for the current status.
    standardsStatusReason :: Prelude.Maybe StandardsStatusReason,
    -- | The ARN of a resource that represents your subscription to a supported
    -- standard.
    standardsSubscriptionArn :: Prelude.Text,
    -- | The ARN of a standard.
    standardsArn :: Prelude.Text,
    -- | A key-value pair of input for the standard.
    standardsInput :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | The status of the standard subscription.
    --
    -- The status values are as follows:
    --
    -- -   @PENDING@ - Standard is in the process of being enabled.
    --
    -- -   @READY@ - Standard is enabled.
    --
    -- -   @INCOMPLETE@ - Standard could not be enabled completely. Some
    --     controls may not be available.
    --
    -- -   @DELETING@ - Standard is in the process of being disabled.
    --
    -- -   @FAILED@ - Standard could not be disabled.
    standardsStatus :: StandardsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StandardsSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'standardsStatusReason', 'standardsSubscription_standardsStatusReason' - The reason for the current status.
--
-- 'standardsSubscriptionArn', 'standardsSubscription_standardsSubscriptionArn' - The ARN of a resource that represents your subscription to a supported
-- standard.
--
-- 'standardsArn', 'standardsSubscription_standardsArn' - The ARN of a standard.
--
-- 'standardsInput', 'standardsSubscription_standardsInput' - A key-value pair of input for the standard.
--
-- 'standardsStatus', 'standardsSubscription_standardsStatus' - The status of the standard subscription.
--
-- The status values are as follows:
--
-- -   @PENDING@ - Standard is in the process of being enabled.
--
-- -   @READY@ - Standard is enabled.
--
-- -   @INCOMPLETE@ - Standard could not be enabled completely. Some
--     controls may not be available.
--
-- -   @DELETING@ - Standard is in the process of being disabled.
--
-- -   @FAILED@ - Standard could not be disabled.
newStandardsSubscription ::
  -- | 'standardsSubscriptionArn'
  Prelude.Text ->
  -- | 'standardsArn'
  Prelude.Text ->
  -- | 'standardsStatus'
  StandardsStatus ->
  StandardsSubscription
newStandardsSubscription
  pStandardsSubscriptionArn_
  pStandardsArn_
  pStandardsStatus_ =
    StandardsSubscription'
      { standardsStatusReason =
          Prelude.Nothing,
        standardsSubscriptionArn =
          pStandardsSubscriptionArn_,
        standardsArn = pStandardsArn_,
        standardsInput = Prelude.mempty,
        standardsStatus = pStandardsStatus_
      }

-- | The reason for the current status.
standardsSubscription_standardsStatusReason :: Lens.Lens' StandardsSubscription (Prelude.Maybe StandardsStatusReason)
standardsSubscription_standardsStatusReason = Lens.lens (\StandardsSubscription' {standardsStatusReason} -> standardsStatusReason) (\s@StandardsSubscription' {} a -> s {standardsStatusReason = a} :: StandardsSubscription)

-- | The ARN of a resource that represents your subscription to a supported
-- standard.
standardsSubscription_standardsSubscriptionArn :: Lens.Lens' StandardsSubscription Prelude.Text
standardsSubscription_standardsSubscriptionArn = Lens.lens (\StandardsSubscription' {standardsSubscriptionArn} -> standardsSubscriptionArn) (\s@StandardsSubscription' {} a -> s {standardsSubscriptionArn = a} :: StandardsSubscription)

-- | The ARN of a standard.
standardsSubscription_standardsArn :: Lens.Lens' StandardsSubscription Prelude.Text
standardsSubscription_standardsArn = Lens.lens (\StandardsSubscription' {standardsArn} -> standardsArn) (\s@StandardsSubscription' {} a -> s {standardsArn = a} :: StandardsSubscription)

-- | A key-value pair of input for the standard.
standardsSubscription_standardsInput :: Lens.Lens' StandardsSubscription (Prelude.HashMap Prelude.Text Prelude.Text)
standardsSubscription_standardsInput = Lens.lens (\StandardsSubscription' {standardsInput} -> standardsInput) (\s@StandardsSubscription' {} a -> s {standardsInput = a} :: StandardsSubscription) Prelude.. Lens.coerced

-- | The status of the standard subscription.
--
-- The status values are as follows:
--
-- -   @PENDING@ - Standard is in the process of being enabled.
--
-- -   @READY@ - Standard is enabled.
--
-- -   @INCOMPLETE@ - Standard could not be enabled completely. Some
--     controls may not be available.
--
-- -   @DELETING@ - Standard is in the process of being disabled.
--
-- -   @FAILED@ - Standard could not be disabled.
standardsSubscription_standardsStatus :: Lens.Lens' StandardsSubscription StandardsStatus
standardsSubscription_standardsStatus = Lens.lens (\StandardsSubscription' {standardsStatus} -> standardsStatus) (\s@StandardsSubscription' {} a -> s {standardsStatus = a} :: StandardsSubscription)

instance Data.FromJSON StandardsSubscription where
  parseJSON =
    Data.withObject
      "StandardsSubscription"
      ( \x ->
          StandardsSubscription'
            Prelude.<$> (x Data..:? "StandardsStatusReason")
            Prelude.<*> (x Data..: "StandardsSubscriptionArn")
            Prelude.<*> (x Data..: "StandardsArn")
            Prelude.<*> (x Data..:? "StandardsInput" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "StandardsStatus")
      )

instance Prelude.Hashable StandardsSubscription where
  hashWithSalt _salt StandardsSubscription' {..} =
    _salt
      `Prelude.hashWithSalt` standardsStatusReason
      `Prelude.hashWithSalt` standardsSubscriptionArn
      `Prelude.hashWithSalt` standardsArn
      `Prelude.hashWithSalt` standardsInput
      `Prelude.hashWithSalt` standardsStatus

instance Prelude.NFData StandardsSubscription where
  rnf StandardsSubscription' {..} =
    Prelude.rnf standardsStatusReason `Prelude.seq`
      Prelude.rnf standardsSubscriptionArn `Prelude.seq`
        Prelude.rnf standardsArn `Prelude.seq`
          Prelude.rnf standardsInput `Prelude.seq`
            Prelude.rnf standardsStatus
