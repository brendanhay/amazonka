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
-- Module      : Amazonka.CertificateManagerPCA.Types.EdiPartyName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.EdiPartyName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Electronic Data Interchange (EDI) entity as described in as
-- defined in
-- <https://datatracker.ietf.org/doc/html/rfc5280 Subject Alternative Name>
-- in RFC 5280.
--
-- /See:/ 'newEdiPartyName' smart constructor.
data EdiPartyName = EdiPartyName'
  { -- | Specifies the name assigner.
    nameAssigner :: Prelude.Maybe Prelude.Text,
    -- | Specifies the party name.
    partyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON EdiPartyName where
  parseJSON =
    Data.withObject
      "EdiPartyName"
      ( \x ->
          EdiPartyName'
            Prelude.<$> (x Data..:? "NameAssigner")
            Prelude.<*> (x Data..: "PartyName")
      )

instance Prelude.Hashable EdiPartyName where
  hashWithSalt _salt EdiPartyName' {..} =
    _salt
      `Prelude.hashWithSalt` nameAssigner
      `Prelude.hashWithSalt` partyName

instance Prelude.NFData EdiPartyName where
  rnf EdiPartyName' {..} =
    Prelude.rnf nameAssigner `Prelude.seq`
      Prelude.rnf partyName

instance Data.ToJSON EdiPartyName where
  toJSON EdiPartyName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NameAssigner" Data..=) Prelude.<$> nameAssigner,
            Prelude.Just ("PartyName" Data..= partyName)
          ]
      )
