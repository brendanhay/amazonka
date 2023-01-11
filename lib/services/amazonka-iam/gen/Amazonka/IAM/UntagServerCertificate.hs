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
-- Module      : Amazonka.IAM.UntagServerCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified tags from the IAM server certificate. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- For certificates in a Region supported by Certificate Manager (ACM), we
-- recommend that you don\'t use IAM server certificates. Instead, use ACM
-- to provision, manage, and deploy your server certificates. For more
-- information about IAM server certificates,
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/.
module Amazonka.IAM.UntagServerCertificate
  ( -- * Creating a Request
    UntagServerCertificate (..),
    newUntagServerCertificate,

    -- * Request Lenses
    untagServerCertificate_serverCertificateName,
    untagServerCertificate_tagKeys,

    -- * Destructuring the Response
    UntagServerCertificateResponse (..),
    newUntagServerCertificateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUntagServerCertificate' smart constructor.
data UntagServerCertificate = UntagServerCertificate'
  { -- | The name of the IAM server certificate from which you want to remove
    -- tags.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serverCertificateName :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified IAM server certificate.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateName', 'untagServerCertificate_serverCertificateName' - The name of the IAM server certificate from which you want to remove
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'tagKeys', 'untagServerCertificate_tagKeys' - A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified IAM server certificate.
newUntagServerCertificate ::
  -- | 'serverCertificateName'
  Prelude.Text ->
  UntagServerCertificate
newUntagServerCertificate pServerCertificateName_ =
  UntagServerCertificate'
    { serverCertificateName =
        pServerCertificateName_,
      tagKeys = Prelude.mempty
    }

-- | The name of the IAM server certificate from which you want to remove
-- tags.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
untagServerCertificate_serverCertificateName :: Lens.Lens' UntagServerCertificate Prelude.Text
untagServerCertificate_serverCertificateName = Lens.lens (\UntagServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@UntagServerCertificate' {} a -> s {serverCertificateName = a} :: UntagServerCertificate)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified IAM server certificate.
untagServerCertificate_tagKeys :: Lens.Lens' UntagServerCertificate [Prelude.Text]
untagServerCertificate_tagKeys = Lens.lens (\UntagServerCertificate' {tagKeys} -> tagKeys) (\s@UntagServerCertificate' {} a -> s {tagKeys = a} :: UntagServerCertificate) Prelude.. Lens.coerced

instance Core.AWSRequest UntagServerCertificate where
  type
    AWSResponse UntagServerCertificate =
      UntagServerCertificateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      UntagServerCertificateResponse'

instance Prelude.Hashable UntagServerCertificate where
  hashWithSalt _salt UntagServerCertificate' {..} =
    _salt `Prelude.hashWithSalt` serverCertificateName
      `Prelude.hashWithSalt` tagKeys

instance Prelude.NFData UntagServerCertificate where
  rnf UntagServerCertificate' {..} =
    Prelude.rnf serverCertificateName
      `Prelude.seq` Prelude.rnf tagKeys

instance Data.ToHeaders UntagServerCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UntagServerCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery UntagServerCertificate where
  toQuery UntagServerCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UntagServerCertificate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "ServerCertificateName"
          Data.=: serverCertificateName,
        "TagKeys" Data.=: Data.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagServerCertificateResponse' smart constructor.
data UntagServerCertificateResponse = UntagServerCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UntagServerCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUntagServerCertificateResponse ::
  UntagServerCertificateResponse
newUntagServerCertificateResponse =
  UntagServerCertificateResponse'

instance
  Prelude.NFData
    UntagServerCertificateResponse
  where
  rnf _ = ()
