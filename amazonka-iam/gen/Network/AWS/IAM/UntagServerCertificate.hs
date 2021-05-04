{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IAM.UntagServerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- For certificates in a Region supported by AWS Certificate Manager (ACM),
-- we recommend that you don\'t use IAM server certificates. Instead, use
-- ACM to provision, manage, and deploy your server certificates. For more
-- information about IAM server certificates,
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UntagServerCertificate
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

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUntagServerCertificate' smart constructor.
data UntagServerCertificate = UntagServerCertificate'
  { -- | The name of the IAM server certificate from which you want to remove
    -- tags.
    --
    -- This parameter accepts (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- that consist of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: =,.\@-
    serverCertificateName :: Prelude.Text,
    -- | A list of key names as a simple array of strings. The tags with matching
    -- keys are removed from the specified IAM server certificate.
    tagKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
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
-- This parameter accepts (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- that consist of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: =,.\@-
untagServerCertificate_serverCertificateName :: Lens.Lens' UntagServerCertificate Prelude.Text
untagServerCertificate_serverCertificateName = Lens.lens (\UntagServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@UntagServerCertificate' {} a -> s {serverCertificateName = a} :: UntagServerCertificate)

-- | A list of key names as a simple array of strings. The tags with matching
-- keys are removed from the specified IAM server certificate.
untagServerCertificate_tagKeys :: Lens.Lens' UntagServerCertificate [Prelude.Text]
untagServerCertificate_tagKeys = Lens.lens (\UntagServerCertificate' {tagKeys} -> tagKeys) (\s@UntagServerCertificate' {} a -> s {tagKeys = a} :: UntagServerCertificate) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UntagServerCertificate where
  type
    Rs UntagServerCertificate =
      UntagServerCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      UntagServerCertificateResponse'

instance Prelude.Hashable UntagServerCertificate

instance Prelude.NFData UntagServerCertificate

instance Prelude.ToHeaders UntagServerCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UntagServerCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UntagServerCertificate where
  toQuery UntagServerCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UntagServerCertificate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "ServerCertificateName"
          Prelude.=: serverCertificateName,
        "TagKeys"
          Prelude.=: Prelude.toQueryList "member" tagKeys
      ]

-- | /See:/ 'newUntagServerCertificateResponse' smart constructor.
data UntagServerCertificateResponse = UntagServerCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
