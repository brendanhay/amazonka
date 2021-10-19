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
-- Module      : Network.AWS.IAM.Types.ServerCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServerCertificate where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types.ServerCertificateMetadata
import Network.AWS.IAM.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a server certificate.
--
-- This data type is used as a response element in the GetServerCertificate
-- operation.
--
-- /See:/ 'newServerCertificate' smart constructor.
data ServerCertificate = ServerCertificate'
  { -- | The contents of the public key certificate chain.
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that are attached to the server certificate. For more
    -- information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The meta information of the server certificate, such as its name, path,
    -- ID, and ARN.
    serverCertificateMetadata :: ServerCertificateMetadata,
    -- | The contents of the public key certificate.
    certificateBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'serverCertificate_certificateChain' - The contents of the public key certificate chain.
--
-- 'tags', 'serverCertificate_tags' - A list of tags that are attached to the server certificate. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'serverCertificateMetadata', 'serverCertificate_serverCertificateMetadata' - The meta information of the server certificate, such as its name, path,
-- ID, and ARN.
--
-- 'certificateBody', 'serverCertificate_certificateBody' - The contents of the public key certificate.
newServerCertificate ::
  -- | 'serverCertificateMetadata'
  ServerCertificateMetadata ->
  -- | 'certificateBody'
  Prelude.Text ->
  ServerCertificate
newServerCertificate
  pServerCertificateMetadata_
  pCertificateBody_ =
    ServerCertificate'
      { certificateChain =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        serverCertificateMetadata =
          pServerCertificateMetadata_,
        certificateBody = pCertificateBody_
      }

-- | The contents of the public key certificate chain.
serverCertificate_certificateChain :: Lens.Lens' ServerCertificate (Prelude.Maybe Prelude.Text)
serverCertificate_certificateChain = Lens.lens (\ServerCertificate' {certificateChain} -> certificateChain) (\s@ServerCertificate' {} a -> s {certificateChain = a} :: ServerCertificate)

-- | A list of tags that are attached to the server certificate. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
serverCertificate_tags :: Lens.Lens' ServerCertificate (Prelude.Maybe [Tag])
serverCertificate_tags = Lens.lens (\ServerCertificate' {tags} -> tags) (\s@ServerCertificate' {} a -> s {tags = a} :: ServerCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The meta information of the server certificate, such as its name, path,
-- ID, and ARN.
serverCertificate_serverCertificateMetadata :: Lens.Lens' ServerCertificate ServerCertificateMetadata
serverCertificate_serverCertificateMetadata = Lens.lens (\ServerCertificate' {serverCertificateMetadata} -> serverCertificateMetadata) (\s@ServerCertificate' {} a -> s {serverCertificateMetadata = a} :: ServerCertificate)

-- | The contents of the public key certificate.
serverCertificate_certificateBody :: Lens.Lens' ServerCertificate Prelude.Text
serverCertificate_certificateBody = Lens.lens (\ServerCertificate' {certificateBody} -> certificateBody) (\s@ServerCertificate' {} a -> s {certificateBody = a} :: ServerCertificate)

instance Core.FromXML ServerCertificate where
  parseXML x =
    ServerCertificate'
      Prelude.<$> (x Core..@? "CertificateChain")
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@ "ServerCertificateMetadata")
      Prelude.<*> (x Core..@ "CertificateBody")

instance Prelude.Hashable ServerCertificate

instance Prelude.NFData ServerCertificate
