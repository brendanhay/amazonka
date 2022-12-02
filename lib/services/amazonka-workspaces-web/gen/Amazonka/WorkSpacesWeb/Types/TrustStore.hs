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
-- Module      : Amazonka.WorkSpacesWeb.Types.TrustStore
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.TrustStore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A trust store that can be associated with a web portal. A trust store
-- contains certificate authority (CA) certificates. Once associated with a
-- web portal, the browser in a streaming session will recognize
-- certificates that have been issued using any of the CAs in the trust
-- store. If your organization has internal websites that use certificates
-- issued by private CAs, you should add the private CA certificate to the
-- trust store.
--
-- /See:/ 'newTrustStore' smart constructor.
data TrustStore = TrustStore'
  { -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Maybe Prelude.Text,
    -- | A list of web portal ARNs that this trust store is associated with.
    associatedPortalArns :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustStore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustStoreArn', 'trustStore_trustStoreArn' - The ARN of the trust store.
--
-- 'associatedPortalArns', 'trustStore_associatedPortalArns' - A list of web portal ARNs that this trust store is associated with.
newTrustStore ::
  TrustStore
newTrustStore =
  TrustStore'
    { trustStoreArn = Prelude.Nothing,
      associatedPortalArns = Prelude.Nothing
    }

-- | The ARN of the trust store.
trustStore_trustStoreArn :: Lens.Lens' TrustStore (Prelude.Maybe Prelude.Text)
trustStore_trustStoreArn = Lens.lens (\TrustStore' {trustStoreArn} -> trustStoreArn) (\s@TrustStore' {} a -> s {trustStoreArn = a} :: TrustStore)

-- | A list of web portal ARNs that this trust store is associated with.
trustStore_associatedPortalArns :: Lens.Lens' TrustStore (Prelude.Maybe [Prelude.Text])
trustStore_associatedPortalArns = Lens.lens (\TrustStore' {associatedPortalArns} -> associatedPortalArns) (\s@TrustStore' {} a -> s {associatedPortalArns = a} :: TrustStore) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TrustStore where
  parseJSON =
    Data.withObject
      "TrustStore"
      ( \x ->
          TrustStore'
            Prelude.<$> (x Data..:? "trustStoreArn")
            Prelude.<*> ( x Data..:? "associatedPortalArns"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TrustStore where
  hashWithSalt _salt TrustStore' {..} =
    _salt `Prelude.hashWithSalt` trustStoreArn
      `Prelude.hashWithSalt` associatedPortalArns

instance Prelude.NFData TrustStore where
  rnf TrustStore' {..} =
    Prelude.rnf trustStoreArn
      `Prelude.seq` Prelude.rnf associatedPortalArns
