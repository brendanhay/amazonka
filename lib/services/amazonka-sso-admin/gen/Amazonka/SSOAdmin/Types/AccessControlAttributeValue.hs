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
-- Module      : Amazonka.SSOAdmin.Types.AccessControlAttributeValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSOAdmin.Types.AccessControlAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The value used for mapping a specified attribute to an identity source.
-- For more information, see
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/attributemappingsconcept.html Attribute mappings>
-- in the /IAM Identity Center User Guide/.
--
-- /See:/ 'newAccessControlAttributeValue' smart constructor.
data AccessControlAttributeValue = AccessControlAttributeValue'
  { -- | The identity source to use when mapping a specified attribute to IAM
    -- Identity Center.
    source :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessControlAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'accessControlAttributeValue_source' - The identity source to use when mapping a specified attribute to IAM
-- Identity Center.
newAccessControlAttributeValue ::
  -- | 'source'
  Prelude.NonEmpty Prelude.Text ->
  AccessControlAttributeValue
newAccessControlAttributeValue pSource_ =
  AccessControlAttributeValue'
    { source =
        Lens.coerced Lens.# pSource_
    }

-- | The identity source to use when mapping a specified attribute to IAM
-- Identity Center.
accessControlAttributeValue_source :: Lens.Lens' AccessControlAttributeValue (Prelude.NonEmpty Prelude.Text)
accessControlAttributeValue_source = Lens.lens (\AccessControlAttributeValue' {source} -> source) (\s@AccessControlAttributeValue' {} a -> s {source = a} :: AccessControlAttributeValue) Prelude.. Lens.coerced

instance Core.FromJSON AccessControlAttributeValue where
  parseJSON =
    Core.withObject
      "AccessControlAttributeValue"
      ( \x ->
          AccessControlAttributeValue'
            Prelude.<$> (x Core..: "Source")
      )

instance Prelude.Hashable AccessControlAttributeValue where
  hashWithSalt _salt AccessControlAttributeValue' {..} =
    _salt `Prelude.hashWithSalt` source

instance Prelude.NFData AccessControlAttributeValue where
  rnf AccessControlAttributeValue' {..} =
    Prelude.rnf source

instance Core.ToJSON AccessControlAttributeValue where
  toJSON AccessControlAttributeValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Source" Core..= source)]
      )
