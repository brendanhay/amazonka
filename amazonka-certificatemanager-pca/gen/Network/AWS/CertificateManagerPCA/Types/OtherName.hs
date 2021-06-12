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
-- Module      : Network.AWS.CertificateManagerPCA.Types.OtherName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.OtherName where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines a custom ASN.1 X.400 @GeneralName@ using an object identifier
-- (OID) and value. The OID must satisfy the regular expression shown
-- below. For more information, see NIST\'s definition of
-- <https://csrc.nist.gov/glossary/term/Object_Identifier Object Identifier (OID)>.
--
-- /See:/ 'newOtherName' smart constructor.
data OtherName = OtherName'
  { -- | Specifies an OID.
    typeId :: Core.Text,
    -- | Specifies an OID value.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OtherName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typeId', 'otherName_typeId' - Specifies an OID.
--
-- 'value', 'otherName_value' - Specifies an OID value.
newOtherName ::
  -- | 'typeId'
  Core.Text ->
  -- | 'value'
  Core.Text ->
  OtherName
newOtherName pTypeId_ pValue_ =
  OtherName' {typeId = pTypeId_, value = pValue_}

-- | Specifies an OID.
otherName_typeId :: Lens.Lens' OtherName Core.Text
otherName_typeId = Lens.lens (\OtherName' {typeId} -> typeId) (\s@OtherName' {} a -> s {typeId = a} :: OtherName)

-- | Specifies an OID value.
otherName_value :: Lens.Lens' OtherName Core.Text
otherName_value = Lens.lens (\OtherName' {value} -> value) (\s@OtherName' {} a -> s {value = a} :: OtherName)

instance Core.FromJSON OtherName where
  parseJSON =
    Core.withObject
      "OtherName"
      ( \x ->
          OtherName'
            Core.<$> (x Core..: "TypeId") Core.<*> (x Core..: "Value")
      )

instance Core.Hashable OtherName

instance Core.NFData OtherName

instance Core.ToJSON OtherName where
  toJSON OtherName' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TypeId" Core..= typeId),
            Core.Just ("Value" Core..= value)
          ]
      )
