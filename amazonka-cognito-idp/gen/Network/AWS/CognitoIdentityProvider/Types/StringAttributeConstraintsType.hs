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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The constraints associated with a string attribute.
--
-- /See:/ 'newStringAttributeConstraintsType' smart constructor.
data StringAttributeConstraintsType = StringAttributeConstraintsType'
  { -- | The minimum length.
    minLength :: Core.Maybe Core.Text,
    -- | The maximum length.
    maxLength :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StringAttributeConstraintsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minLength', 'stringAttributeConstraintsType_minLength' - The minimum length.
--
-- 'maxLength', 'stringAttributeConstraintsType_maxLength' - The maximum length.
newStringAttributeConstraintsType ::
  StringAttributeConstraintsType
newStringAttributeConstraintsType =
  StringAttributeConstraintsType'
    { minLength =
        Core.Nothing,
      maxLength = Core.Nothing
    }

-- | The minimum length.
stringAttributeConstraintsType_minLength :: Lens.Lens' StringAttributeConstraintsType (Core.Maybe Core.Text)
stringAttributeConstraintsType_minLength = Lens.lens (\StringAttributeConstraintsType' {minLength} -> minLength) (\s@StringAttributeConstraintsType' {} a -> s {minLength = a} :: StringAttributeConstraintsType)

-- | The maximum length.
stringAttributeConstraintsType_maxLength :: Lens.Lens' StringAttributeConstraintsType (Core.Maybe Core.Text)
stringAttributeConstraintsType_maxLength = Lens.lens (\StringAttributeConstraintsType' {maxLength} -> maxLength) (\s@StringAttributeConstraintsType' {} a -> s {maxLength = a} :: StringAttributeConstraintsType)

instance Core.FromJSON StringAttributeConstraintsType where
  parseJSON =
    Core.withObject
      "StringAttributeConstraintsType"
      ( \x ->
          StringAttributeConstraintsType'
            Core.<$> (x Core..:? "MinLength")
            Core.<*> (x Core..:? "MaxLength")
      )

instance Core.Hashable StringAttributeConstraintsType

instance Core.NFData StringAttributeConstraintsType

instance Core.ToJSON StringAttributeConstraintsType where
  toJSON StringAttributeConstraintsType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MinLength" Core..=) Core.<$> minLength,
            ("MaxLength" Core..=) Core.<$> maxLength
          ]
      )
