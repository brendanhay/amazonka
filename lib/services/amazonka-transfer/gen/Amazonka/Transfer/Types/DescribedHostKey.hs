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
-- Module      : Amazonka.Transfer.Types.DescribedHostKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.DescribedHostKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.Tag

-- | The details for a server host key.
--
-- /See:/ 'newDescribedHostKey' smart constructor.
data DescribedHostKey = DescribedHostKey'
  { -- | Key-value pairs that can be used to group and search for host keys.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
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
    -- | The text description for this host key.
    description :: Prelude.Maybe Prelude.Text,
    -- | The public key fingerprint, which is a short sequence of bytes used to
    -- identify the longer public key.
    hostKeyFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The date on which the host key was added to the server.
    dateImported :: Prelude.Maybe Core.POSIX,
    -- | The unique Amazon Resource Name (ARN) for the host key.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribedHostKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describedHostKey_tags' - Key-value pairs that can be used to group and search for host keys.
--
-- 'hostKeyId', 'describedHostKey_hostKeyId' - A unique identifier for the host key.
--
-- 'type'', 'describedHostKey_type' - The encryption algorithm that is used for the host key. The @Type@
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
-- 'description', 'describedHostKey_description' - The text description for this host key.
--
-- 'hostKeyFingerprint', 'describedHostKey_hostKeyFingerprint' - The public key fingerprint, which is a short sequence of bytes used to
-- identify the longer public key.
--
-- 'dateImported', 'describedHostKey_dateImported' - The date on which the host key was added to the server.
--
-- 'arn', 'describedHostKey_arn' - The unique Amazon Resource Name (ARN) for the host key.
newDescribedHostKey ::
  -- | 'arn'
  Prelude.Text ->
  DescribedHostKey
newDescribedHostKey pArn_ =
  DescribedHostKey'
    { tags = Prelude.Nothing,
      hostKeyId = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      hostKeyFingerprint = Prelude.Nothing,
      dateImported = Prelude.Nothing,
      arn = pArn_
    }

-- | Key-value pairs that can be used to group and search for host keys.
describedHostKey_tags :: Lens.Lens' DescribedHostKey (Prelude.Maybe (Prelude.NonEmpty Tag))
describedHostKey_tags = Lens.lens (\DescribedHostKey' {tags} -> tags) (\s@DescribedHostKey' {} a -> s {tags = a} :: DescribedHostKey) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the host key.
describedHostKey_hostKeyId :: Lens.Lens' DescribedHostKey (Prelude.Maybe Prelude.Text)
describedHostKey_hostKeyId = Lens.lens (\DescribedHostKey' {hostKeyId} -> hostKeyId) (\s@DescribedHostKey' {} a -> s {hostKeyId = a} :: DescribedHostKey)

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
describedHostKey_type :: Lens.Lens' DescribedHostKey (Prelude.Maybe Prelude.Text)
describedHostKey_type = Lens.lens (\DescribedHostKey' {type'} -> type') (\s@DescribedHostKey' {} a -> s {type' = a} :: DescribedHostKey)

-- | The text description for this host key.
describedHostKey_description :: Lens.Lens' DescribedHostKey (Prelude.Maybe Prelude.Text)
describedHostKey_description = Lens.lens (\DescribedHostKey' {description} -> description) (\s@DescribedHostKey' {} a -> s {description = a} :: DescribedHostKey)

-- | The public key fingerprint, which is a short sequence of bytes used to
-- identify the longer public key.
describedHostKey_hostKeyFingerprint :: Lens.Lens' DescribedHostKey (Prelude.Maybe Prelude.Text)
describedHostKey_hostKeyFingerprint = Lens.lens (\DescribedHostKey' {hostKeyFingerprint} -> hostKeyFingerprint) (\s@DescribedHostKey' {} a -> s {hostKeyFingerprint = a} :: DescribedHostKey)

-- | The date on which the host key was added to the server.
describedHostKey_dateImported :: Lens.Lens' DescribedHostKey (Prelude.Maybe Prelude.UTCTime)
describedHostKey_dateImported = Lens.lens (\DescribedHostKey' {dateImported} -> dateImported) (\s@DescribedHostKey' {} a -> s {dateImported = a} :: DescribedHostKey) Prelude.. Lens.mapping Core._Time

-- | The unique Amazon Resource Name (ARN) for the host key.
describedHostKey_arn :: Lens.Lens' DescribedHostKey Prelude.Text
describedHostKey_arn = Lens.lens (\DescribedHostKey' {arn} -> arn) (\s@DescribedHostKey' {} a -> s {arn = a} :: DescribedHostKey)

instance Core.FromJSON DescribedHostKey where
  parseJSON =
    Core.withObject
      "DescribedHostKey"
      ( \x ->
          DescribedHostKey'
            Prelude.<$> (x Core..:? "Tags")
            Prelude.<*> (x Core..:? "HostKeyId")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "HostKeyFingerprint")
            Prelude.<*> (x Core..:? "DateImported")
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable DescribedHostKey where
  hashWithSalt _salt DescribedHostKey' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hostKeyId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hostKeyFingerprint
      `Prelude.hashWithSalt` dateImported
      `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribedHostKey where
  rnf DescribedHostKey' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf hostKeyId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hostKeyFingerprint
      `Prelude.seq` Prelude.rnf dateImported
      `Prelude.seq` Prelude.rnf arn
