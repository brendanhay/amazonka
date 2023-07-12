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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.SpendLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.SpendLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.SpendLimitName
import qualified Amazonka.Prelude as Prelude

-- | Describes the current Amazon Pinpoint monthly spend limits for sending
-- voice and text messages. For more information on increasing your monthly
-- spend limit, see
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-sms-awssupport-spend-threshold.html Requesting increases to your monthly SMS spending quota for Amazon Pinpoint>
-- in the /Amazon Pinpoint User Guide/.
--
-- /See:/ 'newSpendLimit' smart constructor.
data SpendLimit = SpendLimit'
  { -- | The name for the SpendLimit.
    name :: SpendLimitName,
    -- | The maximum amount of money, in US dollars, that you want to be able to
    -- spend sending messages each month. This value has to be less than or
    -- equal to the amount in @MaxLimit@. To use this custom limit,
    -- @Overridden@ must be set to true.
    enforcedLimit :: Prelude.Integer,
    -- | The maximum amount of money that you are able to spend to send messages
    -- each month, in US dollars.
    maxLimit :: Prelude.Integer,
    -- | When set to @True@, the value that has been specified in the
    -- @EnforcedLimit@ is used to determine the maximum amount in US dollars
    -- that can be spent to send messages each month, in US dollars.
    overridden :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpendLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'spendLimit_name' - The name for the SpendLimit.
--
-- 'enforcedLimit', 'spendLimit_enforcedLimit' - The maximum amount of money, in US dollars, that you want to be able to
-- spend sending messages each month. This value has to be less than or
-- equal to the amount in @MaxLimit@. To use this custom limit,
-- @Overridden@ must be set to true.
--
-- 'maxLimit', 'spendLimit_maxLimit' - The maximum amount of money that you are able to spend to send messages
-- each month, in US dollars.
--
-- 'overridden', 'spendLimit_overridden' - When set to @True@, the value that has been specified in the
-- @EnforcedLimit@ is used to determine the maximum amount in US dollars
-- that can be spent to send messages each month, in US dollars.
newSpendLimit ::
  -- | 'name'
  SpendLimitName ->
  -- | 'enforcedLimit'
  Prelude.Integer ->
  -- | 'maxLimit'
  Prelude.Integer ->
  -- | 'overridden'
  Prelude.Bool ->
  SpendLimit
newSpendLimit
  pName_
  pEnforcedLimit_
  pMaxLimit_
  pOverridden_ =
    SpendLimit'
      { name = pName_,
        enforcedLimit = pEnforcedLimit_,
        maxLimit = pMaxLimit_,
        overridden = pOverridden_
      }

-- | The name for the SpendLimit.
spendLimit_name :: Lens.Lens' SpendLimit SpendLimitName
spendLimit_name = Lens.lens (\SpendLimit' {name} -> name) (\s@SpendLimit' {} a -> s {name = a} :: SpendLimit)

-- | The maximum amount of money, in US dollars, that you want to be able to
-- spend sending messages each month. This value has to be less than or
-- equal to the amount in @MaxLimit@. To use this custom limit,
-- @Overridden@ must be set to true.
spendLimit_enforcedLimit :: Lens.Lens' SpendLimit Prelude.Integer
spendLimit_enforcedLimit = Lens.lens (\SpendLimit' {enforcedLimit} -> enforcedLimit) (\s@SpendLimit' {} a -> s {enforcedLimit = a} :: SpendLimit)

-- | The maximum amount of money that you are able to spend to send messages
-- each month, in US dollars.
spendLimit_maxLimit :: Lens.Lens' SpendLimit Prelude.Integer
spendLimit_maxLimit = Lens.lens (\SpendLimit' {maxLimit} -> maxLimit) (\s@SpendLimit' {} a -> s {maxLimit = a} :: SpendLimit)

-- | When set to @True@, the value that has been specified in the
-- @EnforcedLimit@ is used to determine the maximum amount in US dollars
-- that can be spent to send messages each month, in US dollars.
spendLimit_overridden :: Lens.Lens' SpendLimit Prelude.Bool
spendLimit_overridden = Lens.lens (\SpendLimit' {overridden} -> overridden) (\s@SpendLimit' {} a -> s {overridden = a} :: SpendLimit)

instance Data.FromJSON SpendLimit where
  parseJSON =
    Data.withObject
      "SpendLimit"
      ( \x ->
          SpendLimit'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "EnforcedLimit")
            Prelude.<*> (x Data..: "MaxLimit")
            Prelude.<*> (x Data..: "Overridden")
      )

instance Prelude.Hashable SpendLimit where
  hashWithSalt _salt SpendLimit' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` enforcedLimit
      `Prelude.hashWithSalt` maxLimit
      `Prelude.hashWithSalt` overridden

instance Prelude.NFData SpendLimit where
  rnf SpendLimit' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf enforcedLimit
      `Prelude.seq` Prelude.rnf maxLimit
      `Prelude.seq` Prelude.rnf overridden
