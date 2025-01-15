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
-- Module      : Amazonka.Transfer.Types.ListedHostKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedHostKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns properties of the host key that\'s specified.
--
-- /See:/ 'newListedHostKey' smart constructor.
data ListedHostKey = ListedHostKey'
  { -- | The date on which the host key was added to the server.
    dateImported :: Prelude.Maybe Data.POSIX,
    -- | The current description for the host key. You can change it by calling
    -- the @UpdateHostKey@ operation and providing a new description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The public key fingerprint, which is a short sequence of bytes used to
    -- identify the longer public key.
    fingerprint :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the host key.
    hostKeyId :: Prelude.Maybe Prelude.Text,
    -- | The encryption algorithm that is used for the host key. The @Type@
    -- parameter is specified by using one of the following values:
    --
    -- -   @ssh-rsa@
    --
    -- -   @ssh-ed25519@
    --
    -- -   @ecdsa-sha2-nistp256@
    --
    -- -   @ecdsa-sha2-nistp384@
    --
    -- -   @ecdsa-sha2-nistp521@
    type' :: Prelude.Maybe Prelude.Text,
    -- | The unique Amazon Resource Name (ARN) of the host key.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedHostKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dateImported', 'listedHostKey_dateImported' - The date on which the host key was added to the server.
--
-- 'description', 'listedHostKey_description' - The current description for the host key. You can change it by calling
-- the @UpdateHostKey@ operation and providing a new description.
--
-- 'fingerprint', 'listedHostKey_fingerprint' - The public key fingerprint, which is a short sequence of bytes used to
-- identify the longer public key.
--
-- 'hostKeyId', 'listedHostKey_hostKeyId' - A unique identifier for the host key.
--
-- 'type'', 'listedHostKey_type' - The encryption algorithm that is used for the host key. The @Type@
-- parameter is specified by using one of the following values:
--
-- -   @ssh-rsa@
--
-- -   @ssh-ed25519@
--
-- -   @ecdsa-sha2-nistp256@
--
-- -   @ecdsa-sha2-nistp384@
--
-- -   @ecdsa-sha2-nistp521@
--
-- 'arn', 'listedHostKey_arn' - The unique Amazon Resource Name (ARN) of the host key.
newListedHostKey ::
  -- | 'arn'
  Prelude.Text ->
  ListedHostKey
newListedHostKey pArn_ =
  ListedHostKey'
    { dateImported = Prelude.Nothing,
      description = Prelude.Nothing,
      fingerprint = Prelude.Nothing,
      hostKeyId = Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = pArn_
    }

-- | The date on which the host key was added to the server.
listedHostKey_dateImported :: Lens.Lens' ListedHostKey (Prelude.Maybe Prelude.UTCTime)
listedHostKey_dateImported = Lens.lens (\ListedHostKey' {dateImported} -> dateImported) (\s@ListedHostKey' {} a -> s {dateImported = a} :: ListedHostKey) Prelude.. Lens.mapping Data._Time

-- | The current description for the host key. You can change it by calling
-- the @UpdateHostKey@ operation and providing a new description.
listedHostKey_description :: Lens.Lens' ListedHostKey (Prelude.Maybe Prelude.Text)
listedHostKey_description = Lens.lens (\ListedHostKey' {description} -> description) (\s@ListedHostKey' {} a -> s {description = a} :: ListedHostKey)

-- | The public key fingerprint, which is a short sequence of bytes used to
-- identify the longer public key.
listedHostKey_fingerprint :: Lens.Lens' ListedHostKey (Prelude.Maybe Prelude.Text)
listedHostKey_fingerprint = Lens.lens (\ListedHostKey' {fingerprint} -> fingerprint) (\s@ListedHostKey' {} a -> s {fingerprint = a} :: ListedHostKey)

-- | A unique identifier for the host key.
listedHostKey_hostKeyId :: Lens.Lens' ListedHostKey (Prelude.Maybe Prelude.Text)
listedHostKey_hostKeyId = Lens.lens (\ListedHostKey' {hostKeyId} -> hostKeyId) (\s@ListedHostKey' {} a -> s {hostKeyId = a} :: ListedHostKey)

-- | The encryption algorithm that is used for the host key. The @Type@
-- parameter is specified by using one of the following values:
--
-- -   @ssh-rsa@
--
-- -   @ssh-ed25519@
--
-- -   @ecdsa-sha2-nistp256@
--
-- -   @ecdsa-sha2-nistp384@
--
-- -   @ecdsa-sha2-nistp521@
listedHostKey_type :: Lens.Lens' ListedHostKey (Prelude.Maybe Prelude.Text)
listedHostKey_type = Lens.lens (\ListedHostKey' {type'} -> type') (\s@ListedHostKey' {} a -> s {type' = a} :: ListedHostKey)

-- | The unique Amazon Resource Name (ARN) of the host key.
listedHostKey_arn :: Lens.Lens' ListedHostKey Prelude.Text
listedHostKey_arn = Lens.lens (\ListedHostKey' {arn} -> arn) (\s@ListedHostKey' {} a -> s {arn = a} :: ListedHostKey)

instance Data.FromJSON ListedHostKey where
  parseJSON =
    Data.withObject
      "ListedHostKey"
      ( \x ->
          ListedHostKey'
            Prelude.<$> (x Data..:? "DateImported")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Fingerprint")
            Prelude.<*> (x Data..:? "HostKeyId")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "Arn")
      )

instance Prelude.Hashable ListedHostKey where
  hashWithSalt _salt ListedHostKey' {..} =
    _salt
      `Prelude.hashWithSalt` dateImported
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fingerprint
      `Prelude.hashWithSalt` hostKeyId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListedHostKey where
  rnf ListedHostKey' {..} =
    Prelude.rnf dateImported `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf fingerprint `Prelude.seq`
          Prelude.rnf hostKeyId `Prelude.seq`
            Prelude.rnf type' `Prelude.seq`
              Prelude.rnf arn
