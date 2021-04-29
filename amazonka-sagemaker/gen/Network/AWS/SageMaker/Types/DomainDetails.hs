{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.DomainDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DomainDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.DomainStatus

-- | The domain\'s details.
--
-- /See:/ 'newDomainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { -- | The status.
    status :: Prelude.Maybe DomainStatus,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The domain ID.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The domain\'s Amazon Resource Name (ARN).
    domainArn :: Prelude.Maybe Prelude.Text,
    -- | The domain name.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The domain\'s URL.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DomainDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'domainDetails_status' - The status.
--
-- 'creationTime', 'domainDetails_creationTime' - The creation time.
--
-- 'domainId', 'domainDetails_domainId' - The domain ID.
--
-- 'domainArn', 'domainDetails_domainArn' - The domain\'s Amazon Resource Name (ARN).
--
-- 'domainName', 'domainDetails_domainName' - The domain name.
--
-- 'lastModifiedTime', 'domainDetails_lastModifiedTime' - The last modified time.
--
-- 'url', 'domainDetails_url' - The domain\'s URL.
newDomainDetails ::
  DomainDetails
newDomainDetails =
  DomainDetails'
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      domainId = Prelude.Nothing,
      domainArn = Prelude.Nothing,
      domainName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The status.
domainDetails_status :: Lens.Lens' DomainDetails (Prelude.Maybe DomainStatus)
domainDetails_status = Lens.lens (\DomainDetails' {status} -> status) (\s@DomainDetails' {} a -> s {status = a} :: DomainDetails)

-- | The creation time.
domainDetails_creationTime :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.UTCTime)
domainDetails_creationTime = Lens.lens (\DomainDetails' {creationTime} -> creationTime) (\s@DomainDetails' {} a -> s {creationTime = a} :: DomainDetails) Prelude.. Lens.mapping Prelude._Time

-- | The domain ID.
domainDetails_domainId :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_domainId = Lens.lens (\DomainDetails' {domainId} -> domainId) (\s@DomainDetails' {} a -> s {domainId = a} :: DomainDetails)

-- | The domain\'s Amazon Resource Name (ARN).
domainDetails_domainArn :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_domainArn = Lens.lens (\DomainDetails' {domainArn} -> domainArn) (\s@DomainDetails' {} a -> s {domainArn = a} :: DomainDetails)

-- | The domain name.
domainDetails_domainName :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_domainName = Lens.lens (\DomainDetails' {domainName} -> domainName) (\s@DomainDetails' {} a -> s {domainName = a} :: DomainDetails)

-- | The last modified time.
domainDetails_lastModifiedTime :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.UTCTime)
domainDetails_lastModifiedTime = Lens.lens (\DomainDetails' {lastModifiedTime} -> lastModifiedTime) (\s@DomainDetails' {} a -> s {lastModifiedTime = a} :: DomainDetails) Prelude.. Lens.mapping Prelude._Time

-- | The domain\'s URL.
domainDetails_url :: Lens.Lens' DomainDetails (Prelude.Maybe Prelude.Text)
domainDetails_url = Lens.lens (\DomainDetails' {url} -> url) (\s@DomainDetails' {} a -> s {url = a} :: DomainDetails)

instance Prelude.FromJSON DomainDetails where
  parseJSON =
    Prelude.withObject
      "DomainDetails"
      ( \x ->
          DomainDetails'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "DomainId")
            Prelude.<*> (x Prelude..:? "DomainArn")
            Prelude.<*> (x Prelude..:? "DomainName")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "Url")
      )

instance Prelude.Hashable DomainDetails

instance Prelude.NFData DomainDetails
