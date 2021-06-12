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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.DomainStatus

-- | The domain\'s details.
--
-- /See:/ 'newDomainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { -- | The status.
    status :: Core.Maybe DomainStatus,
    -- | The creation time.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The domain ID.
    domainId :: Core.Maybe Core.Text,
    -- | The domain\'s Amazon Resource Name (ARN).
    domainArn :: Core.Maybe Core.Text,
    -- | The domain name.
    domainName :: Core.Maybe Core.Text,
    -- | The last modified time.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The domain\'s URL.
    url :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      domainId = Core.Nothing,
      domainArn = Core.Nothing,
      domainName = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      url = Core.Nothing
    }

-- | The status.
domainDetails_status :: Lens.Lens' DomainDetails (Core.Maybe DomainStatus)
domainDetails_status = Lens.lens (\DomainDetails' {status} -> status) (\s@DomainDetails' {} a -> s {status = a} :: DomainDetails)

-- | The creation time.
domainDetails_creationTime :: Lens.Lens' DomainDetails (Core.Maybe Core.UTCTime)
domainDetails_creationTime = Lens.lens (\DomainDetails' {creationTime} -> creationTime) (\s@DomainDetails' {} a -> s {creationTime = a} :: DomainDetails) Core.. Lens.mapping Core._Time

-- | The domain ID.
domainDetails_domainId :: Lens.Lens' DomainDetails (Core.Maybe Core.Text)
domainDetails_domainId = Lens.lens (\DomainDetails' {domainId} -> domainId) (\s@DomainDetails' {} a -> s {domainId = a} :: DomainDetails)

-- | The domain\'s Amazon Resource Name (ARN).
domainDetails_domainArn :: Lens.Lens' DomainDetails (Core.Maybe Core.Text)
domainDetails_domainArn = Lens.lens (\DomainDetails' {domainArn} -> domainArn) (\s@DomainDetails' {} a -> s {domainArn = a} :: DomainDetails)

-- | The domain name.
domainDetails_domainName :: Lens.Lens' DomainDetails (Core.Maybe Core.Text)
domainDetails_domainName = Lens.lens (\DomainDetails' {domainName} -> domainName) (\s@DomainDetails' {} a -> s {domainName = a} :: DomainDetails)

-- | The last modified time.
domainDetails_lastModifiedTime :: Lens.Lens' DomainDetails (Core.Maybe Core.UTCTime)
domainDetails_lastModifiedTime = Lens.lens (\DomainDetails' {lastModifiedTime} -> lastModifiedTime) (\s@DomainDetails' {} a -> s {lastModifiedTime = a} :: DomainDetails) Core.. Lens.mapping Core._Time

-- | The domain\'s URL.
domainDetails_url :: Lens.Lens' DomainDetails (Core.Maybe Core.Text)
domainDetails_url = Lens.lens (\DomainDetails' {url} -> url) (\s@DomainDetails' {} a -> s {url = a} :: DomainDetails)

instance Core.FromJSON DomainDetails where
  parseJSON =
    Core.withObject
      "DomainDetails"
      ( \x ->
          DomainDetails'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "DomainId")
            Core.<*> (x Core..:? "DomainArn")
            Core.<*> (x Core..:? "DomainName")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "Url")
      )

instance Core.Hashable DomainDetails

instance Core.NFData DomainDetails
