{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IPRouteInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IPRouteInfo
  ( IPRouteInfo (..),

    -- * Smart constructor
    mkIPRouteInfo,

    -- * Lenses
    iriDirectoryId,
    iriIPRouteStatusReason,
    iriAddedDateTime,
    iriCidrIP,
    iriIPRouteStatusMsg,
    iriDescription,
  )
where

import Network.AWS.DirectoryService.Types.IPRouteStatusMsg
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about one or more IP address blocks.
--
-- /See:/ 'mkIPRouteInfo' smart constructor.
data IPRouteInfo = IPRouteInfo'
  { -- | Identifier (ID) of the directory associated with the IP addresses.
    directoryId :: Lude.Maybe Lude.Text,
    -- | The reason for the IpRouteStatusMsg.
    ipRouteStatusReason :: Lude.Maybe Lude.Text,
    -- | The date and time the address block was added to the directory.
    addedDateTime :: Lude.Maybe Lude.Timestamp,
    -- | IP address block in the 'IpRoute' .
    cidrIP :: Lude.Maybe Lude.Text,
    -- | The status of the IP address block.
    ipRouteStatusMsg :: Lude.Maybe IPRouteStatusMsg,
    -- | Description of the 'IpRouteInfo' .
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IPRouteInfo' with the minimum fields required to make a request.
--
-- * 'directoryId' - Identifier (ID) of the directory associated with the IP addresses.
-- * 'ipRouteStatusReason' - The reason for the IpRouteStatusMsg.
-- * 'addedDateTime' - The date and time the address block was added to the directory.
-- * 'cidrIP' - IP address block in the 'IpRoute' .
-- * 'ipRouteStatusMsg' - The status of the IP address block.
-- * 'description' - Description of the 'IpRouteInfo' .
mkIPRouteInfo ::
  IPRouteInfo
mkIPRouteInfo =
  IPRouteInfo'
    { directoryId = Lude.Nothing,
      ipRouteStatusReason = Lude.Nothing,
      addedDateTime = Lude.Nothing,
      cidrIP = Lude.Nothing,
      ipRouteStatusMsg = Lude.Nothing,
      description = Lude.Nothing
    }

-- | Identifier (ID) of the directory associated with the IP addresses.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriDirectoryId :: Lens.Lens' IPRouteInfo (Lude.Maybe Lude.Text)
iriDirectoryId = Lens.lens (directoryId :: IPRouteInfo -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: IPRouteInfo)
{-# DEPRECATED iriDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The reason for the IpRouteStatusMsg.
--
-- /Note:/ Consider using 'ipRouteStatusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriIPRouteStatusReason :: Lens.Lens' IPRouteInfo (Lude.Maybe Lude.Text)
iriIPRouteStatusReason = Lens.lens (ipRouteStatusReason :: IPRouteInfo -> Lude.Maybe Lude.Text) (\s a -> s {ipRouteStatusReason = a} :: IPRouteInfo)
{-# DEPRECATED iriIPRouteStatusReason "Use generic-lens or generic-optics with 'ipRouteStatusReason' instead." #-}

-- | The date and time the address block was added to the directory.
--
-- /Note:/ Consider using 'addedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriAddedDateTime :: Lens.Lens' IPRouteInfo (Lude.Maybe Lude.Timestamp)
iriAddedDateTime = Lens.lens (addedDateTime :: IPRouteInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {addedDateTime = a} :: IPRouteInfo)
{-# DEPRECATED iriAddedDateTime "Use generic-lens or generic-optics with 'addedDateTime' instead." #-}

-- | IP address block in the 'IpRoute' .
--
-- /Note:/ Consider using 'cidrIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriCidrIP :: Lens.Lens' IPRouteInfo (Lude.Maybe Lude.Text)
iriCidrIP = Lens.lens (cidrIP :: IPRouteInfo -> Lude.Maybe Lude.Text) (\s a -> s {cidrIP = a} :: IPRouteInfo)
{-# DEPRECATED iriCidrIP "Use generic-lens or generic-optics with 'cidrIP' instead." #-}

-- | The status of the IP address block.
--
-- /Note:/ Consider using 'ipRouteStatusMsg' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriIPRouteStatusMsg :: Lens.Lens' IPRouteInfo (Lude.Maybe IPRouteStatusMsg)
iriIPRouteStatusMsg = Lens.lens (ipRouteStatusMsg :: IPRouteInfo -> Lude.Maybe IPRouteStatusMsg) (\s a -> s {ipRouteStatusMsg = a} :: IPRouteInfo)
{-# DEPRECATED iriIPRouteStatusMsg "Use generic-lens or generic-optics with 'ipRouteStatusMsg' instead." #-}

-- | Description of the 'IpRouteInfo' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iriDescription :: Lens.Lens' IPRouteInfo (Lude.Maybe Lude.Text)
iriDescription = Lens.lens (description :: IPRouteInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: IPRouteInfo)
{-# DEPRECATED iriDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON IPRouteInfo where
  parseJSON =
    Lude.withObject
      "IPRouteInfo"
      ( \x ->
          IPRouteInfo'
            Lude.<$> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "IpRouteStatusReason")
            Lude.<*> (x Lude..:? "AddedDateTime")
            Lude.<*> (x Lude..:? "CidrIp")
            Lude.<*> (x Lude..:? "IpRouteStatusMsg")
            Lude.<*> (x Lude..:? "Description")
      )
