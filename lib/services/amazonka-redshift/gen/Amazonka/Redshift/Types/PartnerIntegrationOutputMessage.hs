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
-- Module      : Amazonka.Redshift.Types.PartnerIntegrationOutputMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.PartnerIntegrationOutputMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | /See:/ 'newPartnerIntegrationOutputMessage' smart constructor.
data PartnerIntegrationOutputMessage = PartnerIntegrationOutputMessage'
  { -- | The name of the database that receives data from the partner.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the partner that is authorized to send data.
    partnerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartnerIntegrationOutputMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'partnerIntegrationOutputMessage_databaseName' - The name of the database that receives data from the partner.
--
-- 'partnerName', 'partnerIntegrationOutputMessage_partnerName' - The name of the partner that is authorized to send data.
newPartnerIntegrationOutputMessage ::
  PartnerIntegrationOutputMessage
newPartnerIntegrationOutputMessage =
  PartnerIntegrationOutputMessage'
    { databaseName =
        Prelude.Nothing,
      partnerName = Prelude.Nothing
    }

-- | The name of the database that receives data from the partner.
partnerIntegrationOutputMessage_databaseName :: Lens.Lens' PartnerIntegrationOutputMessage (Prelude.Maybe Prelude.Text)
partnerIntegrationOutputMessage_databaseName = Lens.lens (\PartnerIntegrationOutputMessage' {databaseName} -> databaseName) (\s@PartnerIntegrationOutputMessage' {} a -> s {databaseName = a} :: PartnerIntegrationOutputMessage)

-- | The name of the partner that is authorized to send data.
partnerIntegrationOutputMessage_partnerName :: Lens.Lens' PartnerIntegrationOutputMessage (Prelude.Maybe Prelude.Text)
partnerIntegrationOutputMessage_partnerName = Lens.lens (\PartnerIntegrationOutputMessage' {partnerName} -> partnerName) (\s@PartnerIntegrationOutputMessage' {} a -> s {partnerName = a} :: PartnerIntegrationOutputMessage)

instance Data.FromXML PartnerIntegrationOutputMessage where
  parseXML x =
    PartnerIntegrationOutputMessage'
      Prelude.<$> (x Data..@? "DatabaseName")
      Prelude.<*> (x Data..@? "PartnerName")

instance
  Prelude.Hashable
    PartnerIntegrationOutputMessage
  where
  hashWithSalt
    _salt
    PartnerIntegrationOutputMessage' {..} =
      _salt `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` partnerName

instance
  Prelude.NFData
    PartnerIntegrationOutputMessage
  where
  rnf PartnerIntegrationOutputMessage' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf partnerName
