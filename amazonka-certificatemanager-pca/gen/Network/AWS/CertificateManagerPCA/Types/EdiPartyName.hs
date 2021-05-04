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
-- Module      : Network.AWS.CertificateManagerPCA.Types.EdiPartyName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.EdiPartyName where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Electronic Data Interchange (EDI) entity as described in as
-- defined in
-- <https://tools.ietf.org/html/rfc5280 Subject Alternative Name> in RFC
-- 5280.
--
-- /See:/ 'newEdiPartyName' smart constructor.
data EdiPartyName = EdiPartyName'
  { -- | Specifies the name assigner.
    nameAssigner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the party name.
    partyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EdiPartyName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameAssigner', 'ediPartyName_nameAssigner' - Specifies the name assigner.
--
-- 'partyName', 'ediPartyName_partyName' - Specifies the party name.
newEdiPartyName ::
  -- | 'partyName'
  Prelude.Text ->
  EdiPartyName
newEdiPartyName pPartyName_ =
  EdiPartyName'
    { nameAssigner = Prelude.Nothing,
      partyName = pPartyName_
    }

-- | Specifies the name assigner.
ediPartyName_nameAssigner :: Lens.Lens' EdiPartyName (Prelude.Maybe Prelude.Text)
ediPartyName_nameAssigner = Lens.lens (\EdiPartyName' {nameAssigner} -> nameAssigner) (\s@EdiPartyName' {} a -> s {nameAssigner = a} :: EdiPartyName)

-- | Specifies the party name.
ediPartyName_partyName :: Lens.Lens' EdiPartyName Prelude.Text
ediPartyName_partyName = Lens.lens (\EdiPartyName' {partyName} -> partyName) (\s@EdiPartyName' {} a -> s {partyName = a} :: EdiPartyName)

instance Prelude.FromJSON EdiPartyName where
  parseJSON =
    Prelude.withObject
      "EdiPartyName"
      ( \x ->
          EdiPartyName'
            Prelude.<$> (x Prelude..:? "NameAssigner")
            Prelude.<*> (x Prelude..: "PartyName")
      )

instance Prelude.Hashable EdiPartyName

instance Prelude.NFData EdiPartyName

instance Prelude.ToJSON EdiPartyName where
  toJSON EdiPartyName' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NameAssigner" Prelude..=)
              Prelude.<$> nameAssigner,
            Prelude.Just ("PartyName" Prelude..= partyName)
          ]
      )
