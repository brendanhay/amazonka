-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Trust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Trust
  ( Trust (..),

    -- * Smart constructor
    mkTrust,

    -- * Lenses
    tDirectoryId,
    tTrustState,
    tLastUpdatedDateTime,
    tTrustDirection,
    tStateLastUpdatedDateTime,
    tTrustType,
    tTrustStateReason,
    tSelectiveAuth,
    tRemoteDomainName,
    tTrustId,
    tCreatedDateTime,
  )
where

import Network.AWS.DirectoryService.Types.SelectiveAuth
import Network.AWS.DirectoryService.Types.TrustDirection
import Network.AWS.DirectoryService.Types.TrustState
import Network.AWS.DirectoryService.Types.TrustType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'mkTrust' smart constructor.
data Trust = Trust'
  { directoryId :: Lude.Maybe Lude.Text,
    trustState :: Lude.Maybe TrustState,
    lastUpdatedDateTime :: Lude.Maybe Lude.Timestamp,
    trustDirection :: Lude.Maybe TrustDirection,
    stateLastUpdatedDateTime :: Lude.Maybe Lude.Timestamp,
    trustType :: Lude.Maybe TrustType,
    trustStateReason :: Lude.Maybe Lude.Text,
    selectiveAuth :: Lude.Maybe SelectiveAuth,
    remoteDomainName :: Lude.Maybe Lude.Text,
    trustId :: Lude.Maybe Lude.Text,
    createdDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Trust' with the minimum fields required to make a request.
--
-- * 'createdDateTime' - The date and time that the trust relationship was created.
-- * 'directoryId' - The Directory ID of the AWS directory involved in the trust relationship.
-- * 'lastUpdatedDateTime' - The date and time that the trust relationship was last updated.
-- * 'remoteDomainName' - The Fully Qualified Domain Name (FQDN) of the external domain involved in the trust relationship.
-- * 'selectiveAuth' - Current state of selective authentication for the trust.
-- * 'stateLastUpdatedDateTime' - The date and time that the TrustState was last updated.
-- * 'trustDirection' - The trust relationship direction.
-- * 'trustId' - The unique ID of the trust relationship.
-- * 'trustState' - The trust relationship state.
-- * 'trustStateReason' - The reason for the TrustState.
-- * 'trustType' - The trust relationship type. @Forest@ is the default.
mkTrust ::
  Trust
mkTrust =
  Trust'
    { directoryId = Lude.Nothing,
      trustState = Lude.Nothing,
      lastUpdatedDateTime = Lude.Nothing,
      trustDirection = Lude.Nothing,
      stateLastUpdatedDateTime = Lude.Nothing,
      trustType = Lude.Nothing,
      trustStateReason = Lude.Nothing,
      selectiveAuth = Lude.Nothing,
      remoteDomainName = Lude.Nothing,
      trustId = Lude.Nothing,
      createdDateTime = Lude.Nothing
    }

-- | The Directory ID of the AWS directory involved in the trust relationship.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDirectoryId :: Lens.Lens' Trust (Lude.Maybe Lude.Text)
tDirectoryId = Lens.lens (directoryId :: Trust -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: Trust)
{-# DEPRECATED tDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The trust relationship state.
--
-- /Note:/ Consider using 'trustState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustState :: Lens.Lens' Trust (Lude.Maybe TrustState)
tTrustState = Lens.lens (trustState :: Trust -> Lude.Maybe TrustState) (\s a -> s {trustState = a} :: Trust)
{-# DEPRECATED tTrustState "Use generic-lens or generic-optics with 'trustState' instead." #-}

-- | The date and time that the trust relationship was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tLastUpdatedDateTime :: Lens.Lens' Trust (Lude.Maybe Lude.Timestamp)
tLastUpdatedDateTime = Lens.lens (lastUpdatedDateTime :: Trust -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDateTime = a} :: Trust)
{-# DEPRECATED tLastUpdatedDateTime "Use generic-lens or generic-optics with 'lastUpdatedDateTime' instead." #-}

-- | The trust relationship direction.
--
-- /Note:/ Consider using 'trustDirection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustDirection :: Lens.Lens' Trust (Lude.Maybe TrustDirection)
tTrustDirection = Lens.lens (trustDirection :: Trust -> Lude.Maybe TrustDirection) (\s a -> s {trustDirection = a} :: Trust)
{-# DEPRECATED tTrustDirection "Use generic-lens or generic-optics with 'trustDirection' instead." #-}

-- | The date and time that the TrustState was last updated.
--
-- /Note:/ Consider using 'stateLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tStateLastUpdatedDateTime :: Lens.Lens' Trust (Lude.Maybe Lude.Timestamp)
tStateLastUpdatedDateTime = Lens.lens (stateLastUpdatedDateTime :: Trust -> Lude.Maybe Lude.Timestamp) (\s a -> s {stateLastUpdatedDateTime = a} :: Trust)
{-# DEPRECATED tStateLastUpdatedDateTime "Use generic-lens or generic-optics with 'stateLastUpdatedDateTime' instead." #-}

-- | The trust relationship type. @Forest@ is the default.
--
-- /Note:/ Consider using 'trustType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustType :: Lens.Lens' Trust (Lude.Maybe TrustType)
tTrustType = Lens.lens (trustType :: Trust -> Lude.Maybe TrustType) (\s a -> s {trustType = a} :: Trust)
{-# DEPRECATED tTrustType "Use generic-lens or generic-optics with 'trustType' instead." #-}

-- | The reason for the TrustState.
--
-- /Note:/ Consider using 'trustStateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustStateReason :: Lens.Lens' Trust (Lude.Maybe Lude.Text)
tTrustStateReason = Lens.lens (trustStateReason :: Trust -> Lude.Maybe Lude.Text) (\s a -> s {trustStateReason = a} :: Trust)
{-# DEPRECATED tTrustStateReason "Use generic-lens or generic-optics with 'trustStateReason' instead." #-}

-- | Current state of selective authentication for the trust.
--
-- /Note:/ Consider using 'selectiveAuth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSelectiveAuth :: Lens.Lens' Trust (Lude.Maybe SelectiveAuth)
tSelectiveAuth = Lens.lens (selectiveAuth :: Trust -> Lude.Maybe SelectiveAuth) (\s a -> s {selectiveAuth = a} :: Trust)
{-# DEPRECATED tSelectiveAuth "Use generic-lens or generic-optics with 'selectiveAuth' instead." #-}

-- | The Fully Qualified Domain Name (FQDN) of the external domain involved in the trust relationship.
--
-- /Note:/ Consider using 'remoteDomainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRemoteDomainName :: Lens.Lens' Trust (Lude.Maybe Lude.Text)
tRemoteDomainName = Lens.lens (remoteDomainName :: Trust -> Lude.Maybe Lude.Text) (\s a -> s {remoteDomainName = a} :: Trust)
{-# DEPRECATED tRemoteDomainName "Use generic-lens or generic-optics with 'remoteDomainName' instead." #-}

-- | The unique ID of the trust relationship.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTrustId :: Lens.Lens' Trust (Lude.Maybe Lude.Text)
tTrustId = Lens.lens (trustId :: Trust -> Lude.Maybe Lude.Text) (\s a -> s {trustId = a} :: Trust)
{-# DEPRECATED tTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | The date and time that the trust relationship was created.
--
-- /Note:/ Consider using 'createdDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tCreatedDateTime :: Lens.Lens' Trust (Lude.Maybe Lude.Timestamp)
tCreatedDateTime = Lens.lens (createdDateTime :: Trust -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDateTime = a} :: Trust)
{-# DEPRECATED tCreatedDateTime "Use generic-lens or generic-optics with 'createdDateTime' instead." #-}

instance Lude.FromJSON Trust where
  parseJSON =
    Lude.withObject
      "Trust"
      ( \x ->
          Trust'
            Lude.<$> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "TrustState")
            Lude.<*> (x Lude..:? "LastUpdatedDateTime")
            Lude.<*> (x Lude..:? "TrustDirection")
            Lude.<*> (x Lude..:? "StateLastUpdatedDateTime")
            Lude.<*> (x Lude..:? "TrustType")
            Lude.<*> (x Lude..:? "TrustStateReason")
            Lude.<*> (x Lude..:? "SelectiveAuth")
            Lude.<*> (x Lude..:? "RemoteDomainName")
            Lude.<*> (x Lude..:? "TrustId")
            Lude.<*> (x Lude..:? "CreatedDateTime")
      )
