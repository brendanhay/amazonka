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
-- Module      : Network.AWS.SSOAdmin.Types.AccessControlAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSOAdmin.Types.AccessControlAttributeValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The value used for mapping a specified attribute to an identity source.
--
-- /See:/ 'newAccessControlAttributeValue' smart constructor.
data AccessControlAttributeValue = AccessControlAttributeValue'
  { -- | The identity source to use when mapping a specified attribute to Amazon
    -- Web Services SSO.
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
-- 'source', 'accessControlAttributeValue_source' - The identity source to use when mapping a specified attribute to Amazon
-- Web Services SSO.
newAccessControlAttributeValue ::
  -- | 'source'
  Prelude.NonEmpty Prelude.Text ->
  AccessControlAttributeValue
newAccessControlAttributeValue pSource_ =
  AccessControlAttributeValue'
    { source =
        Lens.coerced Lens.# pSource_
    }

-- | The identity source to use when mapping a specified attribute to Amazon
-- Web Services SSO.
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

instance Prelude.Hashable AccessControlAttributeValue

instance Prelude.NFData AccessControlAttributeValue

instance Core.ToJSON AccessControlAttributeValue where
  toJSON AccessControlAttributeValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Source" Core..= source)]
      )
