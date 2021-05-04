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
-- Module      : Network.AWS.ElasticSearch.Types.AdditionalLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdditionalLimit where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | List of limits that are specific to a given InstanceType and for each of
-- it\'s @ InstanceRole @ .
--
-- /See:/ 'newAdditionalLimit' smart constructor.
data AdditionalLimit = AdditionalLimit'
  { -- | Value for given @ AdditionalLimit$LimitName @ .
    limitValues :: Prelude.Maybe [Prelude.Text],
    -- | Name of Additional Limit is specific to a given InstanceType and for
    -- each of it\'s @ InstanceRole @ etc.
    -- Attributes and their details:
    --
    -- -   MaximumNumberOfDataNodesSupported
    -- -   MaximumNumberOfDataNodesWithoutMasterNode
    limitName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdditionalLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitValues', 'additionalLimit_limitValues' - Value for given @ AdditionalLimit$LimitName @ .
--
-- 'limitName', 'additionalLimit_limitName' - Name of Additional Limit is specific to a given InstanceType and for
-- each of it\'s @ InstanceRole @ etc.
-- Attributes and their details:
--
-- -   MaximumNumberOfDataNodesSupported
-- -   MaximumNumberOfDataNodesWithoutMasterNode
newAdditionalLimit ::
  AdditionalLimit
newAdditionalLimit =
  AdditionalLimit'
    { limitValues = Prelude.Nothing,
      limitName = Prelude.Nothing
    }

-- | Value for given @ AdditionalLimit$LimitName @ .
additionalLimit_limitValues :: Lens.Lens' AdditionalLimit (Prelude.Maybe [Prelude.Text])
additionalLimit_limitValues = Lens.lens (\AdditionalLimit' {limitValues} -> limitValues) (\s@AdditionalLimit' {} a -> s {limitValues = a} :: AdditionalLimit) Prelude.. Lens.mapping Prelude._Coerce

-- | Name of Additional Limit is specific to a given InstanceType and for
-- each of it\'s @ InstanceRole @ etc.
-- Attributes and their details:
--
-- -   MaximumNumberOfDataNodesSupported
-- -   MaximumNumberOfDataNodesWithoutMasterNode
additionalLimit_limitName :: Lens.Lens' AdditionalLimit (Prelude.Maybe Prelude.Text)
additionalLimit_limitName = Lens.lens (\AdditionalLimit' {limitName} -> limitName) (\s@AdditionalLimit' {} a -> s {limitName = a} :: AdditionalLimit)

instance Prelude.FromJSON AdditionalLimit where
  parseJSON =
    Prelude.withObject
      "AdditionalLimit"
      ( \x ->
          AdditionalLimit'
            Prelude.<$> ( x Prelude..:? "LimitValues"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "LimitName")
      )

instance Prelude.Hashable AdditionalLimit

instance Prelude.NFData AdditionalLimit
