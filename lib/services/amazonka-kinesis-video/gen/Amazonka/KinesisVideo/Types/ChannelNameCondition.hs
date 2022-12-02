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
-- Module      : Amazonka.KinesisVideo.Types.ChannelNameCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.ChannelNameCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.ComparisonOperator
import qualified Amazonka.Prelude as Prelude

-- | An optional input parameter for the @ListSignalingChannels@ API. When
-- this parameter is specified while invoking @ListSignalingChannels@, the
-- API returns only the channels that satisfy a condition specified in
-- @ChannelNameCondition@.
--
-- /See:/ 'newChannelNameCondition' smart constructor.
data ChannelNameCondition = ChannelNameCondition'
  { -- | A value to compare.
    comparisonValue :: Prelude.Maybe Prelude.Text,
    -- | A comparison operator. Currently, you can only specify the @BEGINS_WITH@
    -- operator, which finds signaling channels whose names begin with a given
    -- prefix.
    comparisonOperator :: Prelude.Maybe ComparisonOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelNameCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comparisonValue', 'channelNameCondition_comparisonValue' - A value to compare.
--
-- 'comparisonOperator', 'channelNameCondition_comparisonOperator' - A comparison operator. Currently, you can only specify the @BEGINS_WITH@
-- operator, which finds signaling channels whose names begin with a given
-- prefix.
newChannelNameCondition ::
  ChannelNameCondition
newChannelNameCondition =
  ChannelNameCondition'
    { comparisonValue =
        Prelude.Nothing,
      comparisonOperator = Prelude.Nothing
    }

-- | A value to compare.
channelNameCondition_comparisonValue :: Lens.Lens' ChannelNameCondition (Prelude.Maybe Prelude.Text)
channelNameCondition_comparisonValue = Lens.lens (\ChannelNameCondition' {comparisonValue} -> comparisonValue) (\s@ChannelNameCondition' {} a -> s {comparisonValue = a} :: ChannelNameCondition)

-- | A comparison operator. Currently, you can only specify the @BEGINS_WITH@
-- operator, which finds signaling channels whose names begin with a given
-- prefix.
channelNameCondition_comparisonOperator :: Lens.Lens' ChannelNameCondition (Prelude.Maybe ComparisonOperator)
channelNameCondition_comparisonOperator = Lens.lens (\ChannelNameCondition' {comparisonOperator} -> comparisonOperator) (\s@ChannelNameCondition' {} a -> s {comparisonOperator = a} :: ChannelNameCondition)

instance Prelude.Hashable ChannelNameCondition where
  hashWithSalt _salt ChannelNameCondition' {..} =
    _salt `Prelude.hashWithSalt` comparisonValue
      `Prelude.hashWithSalt` comparisonOperator

instance Prelude.NFData ChannelNameCondition where
  rnf ChannelNameCondition' {..} =
    Prelude.rnf comparisonValue
      `Prelude.seq` Prelude.rnf comparisonOperator

instance Data.ToJSON ChannelNameCondition where
  toJSON ChannelNameCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComparisonValue" Data..=)
              Prelude.<$> comparisonValue,
            ("ComparisonOperator" Data..=)
              Prelude.<$> comparisonOperator
          ]
      )
