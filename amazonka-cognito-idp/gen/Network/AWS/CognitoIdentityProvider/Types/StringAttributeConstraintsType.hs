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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.StringAttributeConstraintsType where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The constraints associated with a string attribute.
--
-- /See:/ 'newStringAttributeConstraintsType' smart constructor.
data StringAttributeConstraintsType = StringAttributeConstraintsType'
  { -- | The minimum length.
    minLength :: Prelude.Maybe Prelude.Text,
    -- | The maximum length.
    maxLength :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      maxLength = Prelude.Nothing
    }

-- | The minimum length.
stringAttributeConstraintsType_minLength :: Lens.Lens' StringAttributeConstraintsType (Prelude.Maybe Prelude.Text)
stringAttributeConstraintsType_minLength = Lens.lens (\StringAttributeConstraintsType' {minLength} -> minLength) (\s@StringAttributeConstraintsType' {} a -> s {minLength = a} :: StringAttributeConstraintsType)

-- | The maximum length.
stringAttributeConstraintsType_maxLength :: Lens.Lens' StringAttributeConstraintsType (Prelude.Maybe Prelude.Text)
stringAttributeConstraintsType_maxLength = Lens.lens (\StringAttributeConstraintsType' {maxLength} -> maxLength) (\s@StringAttributeConstraintsType' {} a -> s {maxLength = a} :: StringAttributeConstraintsType)

instance
  Prelude.FromJSON
    StringAttributeConstraintsType
  where
  parseJSON =
    Prelude.withObject
      "StringAttributeConstraintsType"
      ( \x ->
          StringAttributeConstraintsType'
            Prelude.<$> (x Prelude..:? "MinLength")
            Prelude.<*> (x Prelude..:? "MaxLength")
      )

instance
  Prelude.Hashable
    StringAttributeConstraintsType

instance
  Prelude.NFData
    StringAttributeConstraintsType

instance
  Prelude.ToJSON
    StringAttributeConstraintsType
  where
  toJSON StringAttributeConstraintsType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MinLength" Prelude..=) Prelude.<$> minLength,
            ("MaxLength" Prelude..=) Prelude.<$> maxLength
          ]
      )
