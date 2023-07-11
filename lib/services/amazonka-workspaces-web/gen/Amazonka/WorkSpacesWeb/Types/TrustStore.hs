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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | A list of web portal ARNs that this trust store is associated with.
    associatedPortalArns :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the trust store.
    trustStoreArn :: Prelude.Maybe Prelude.Text
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
-- 'associatedPortalArns', 'trustStore_associatedPortalArns' - A list of web portal ARNs that this trust store is associated with.
--
-- 'trustStoreArn', 'trustStore_trustStoreArn' - The ARN of the trust store.
newTrustStore ::
  TrustStore
newTrustStore =
  TrustStore'
    { associatedPortalArns = Prelude.Nothing,
      trustStoreArn = Prelude.Nothing
    }

-- | A list of web portal ARNs that this trust store is associated with.
trustStore_associatedPortalArns :: Lens.Lens' TrustStore (Prelude.Maybe [Prelude.Text])
trustStore_associatedPortalArns = Lens.lens (\TrustStore' {associatedPortalArns} -> associatedPortalArns) (\s@TrustStore' {} a -> s {associatedPortalArns = a} :: TrustStore) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the trust store.
trustStore_trustStoreArn :: Lens.Lens' TrustStore (Prelude.Maybe Prelude.Text)
trustStore_trustStoreArn = Lens.lens (\TrustStore' {trustStoreArn} -> trustStoreArn) (\s@TrustStore' {} a -> s {trustStoreArn = a} :: TrustStore)

instance Data.FromJSON TrustStore where
  parseJSON =
    Data.withObject
      "TrustStore"
      ( \x ->
          TrustStore'
            Prelude.<$> ( x
                            Data..:? "associatedPortalArns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "trustStoreArn")
      )

instance Prelude.Hashable TrustStore where
  hashWithSalt _salt TrustStore' {..} =
    _salt
      `Prelude.hashWithSalt` associatedPortalArns
      `Prelude.hashWithSalt` trustStoreArn

instance Prelude.NFData TrustStore where
  rnf TrustStore' {..} =
    Prelude.rnf associatedPortalArns
      `Prelude.seq` Prelude.rnf trustStoreArn
