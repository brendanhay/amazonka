{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IAM.DeleteSAMLProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SAML provider resource in IAM.
--
-- Deleting the provider resource from IAM does not update any roles that
-- reference the SAML provider resource\'s ARN as a principal in their
-- trust policies. Any attempt to assume a role that references a
-- non-existent provider resource ARN fails.
--
-- This operation requires
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
module Amazonka.IAM.DeleteSAMLProvider
  ( -- * Creating a Request
    DeleteSAMLProvider (..),
    newDeleteSAMLProvider,

    -- * Request Lenses
    deleteSAMLProvider_sAMLProviderArn,

    -- * Destructuring the Response
    DeleteSAMLProviderResponse (..),
    newDeleteSAMLProviderResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSAMLProvider' smart constructor.
data DeleteSAMLProvider = DeleteSAMLProvider'
  { -- | The Amazon Resource Name (ARN) of the SAML provider to delete.
    sAMLProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSAMLProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sAMLProviderArn', 'deleteSAMLProvider_sAMLProviderArn' - The Amazon Resource Name (ARN) of the SAML provider to delete.
newDeleteSAMLProvider ::
  -- | 'sAMLProviderArn'
  Prelude.Text ->
  DeleteSAMLProvider
newDeleteSAMLProvider pSAMLProviderArn_ =
  DeleteSAMLProvider'
    { sAMLProviderArn =
        pSAMLProviderArn_
    }

-- | The Amazon Resource Name (ARN) of the SAML provider to delete.
deleteSAMLProvider_sAMLProviderArn :: Lens.Lens' DeleteSAMLProvider Prelude.Text
deleteSAMLProvider_sAMLProviderArn = Lens.lens (\DeleteSAMLProvider' {sAMLProviderArn} -> sAMLProviderArn) (\s@DeleteSAMLProvider' {} a -> s {sAMLProviderArn = a} :: DeleteSAMLProvider)

instance Core.AWSRequest DeleteSAMLProvider where
  type
    AWSResponse DeleteSAMLProvider =
      DeleteSAMLProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteSAMLProviderResponse'

instance Prelude.Hashable DeleteSAMLProvider where
  hashWithSalt _salt DeleteSAMLProvider' {..} =
    _salt `Prelude.hashWithSalt` sAMLProviderArn

instance Prelude.NFData DeleteSAMLProvider where
  rnf DeleteSAMLProvider' {..} =
    Prelude.rnf sAMLProviderArn

instance Data.ToHeaders DeleteSAMLProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSAMLProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSAMLProvider where
  toQuery DeleteSAMLProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteSAMLProvider" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "SAMLProviderArn" Data.=: sAMLProviderArn
      ]

-- | /See:/ 'newDeleteSAMLProviderResponse' smart constructor.
data DeleteSAMLProviderResponse = DeleteSAMLProviderResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSAMLProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSAMLProviderResponse ::
  DeleteSAMLProviderResponse
newDeleteSAMLProviderResponse =
  DeleteSAMLProviderResponse'

instance Prelude.NFData DeleteSAMLProviderResponse where
  rnf _ = ()
