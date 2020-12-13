{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DomainDetails
  ( DomainDetails (..),

    -- * Smart constructor
    mkDomainDetails,

    -- * Lenses
    ddCreationTime,
    ddStatus,
    ddDomainARN,
    ddURL,
    ddLastModifiedTime,
    ddDomainName,
    ddDomainId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.DomainStatus

-- | The domain's details.
--
-- /See:/ 'mkDomainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { -- | The creation time.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | The status.
    status :: Lude.Maybe DomainStatus,
    -- | The domain's Amazon Resource Name (ARN).
    domainARN :: Lude.Maybe Lude.Text,
    -- | The domain's URL.
    url :: Lude.Maybe Lude.Text,
    -- | The last modified time.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The domain name.
    domainName :: Lude.Maybe Lude.Text,
    -- | The domain ID.
    domainId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainDetails' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time.
-- * 'status' - The status.
-- * 'domainARN' - The domain's Amazon Resource Name (ARN).
-- * 'url' - The domain's URL.
-- * 'lastModifiedTime' - The last modified time.
-- * 'domainName' - The domain name.
-- * 'domainId' - The domain ID.
mkDomainDetails ::
  DomainDetails
mkDomainDetails =
  DomainDetails'
    { creationTime = Lude.Nothing,
      status = Lude.Nothing,
      domainARN = Lude.Nothing,
      url = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      domainName = Lude.Nothing,
      domainId = Lude.Nothing
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddCreationTime :: Lens.Lens' DomainDetails (Lude.Maybe Lude.Timestamp)
ddCreationTime = Lens.lens (creationTime :: DomainDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DomainDetails)
{-# DEPRECATED ddCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddStatus :: Lens.Lens' DomainDetails (Lude.Maybe DomainStatus)
ddStatus = Lens.lens (status :: DomainDetails -> Lude.Maybe DomainStatus) (\s a -> s {status = a} :: DomainDetails)
{-# DEPRECATED ddStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The domain's Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'domainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainARN :: Lens.Lens' DomainDetails (Lude.Maybe Lude.Text)
ddDomainARN = Lens.lens (domainARN :: DomainDetails -> Lude.Maybe Lude.Text) (\s a -> s {domainARN = a} :: DomainDetails)
{-# DEPRECATED ddDomainARN "Use generic-lens or generic-optics with 'domainARN' instead." #-}

-- | The domain's URL.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddURL :: Lens.Lens' DomainDetails (Lude.Maybe Lude.Text)
ddURL = Lens.lens (url :: DomainDetails -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DomainDetails)
{-# DEPRECATED ddURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddLastModifiedTime :: Lens.Lens' DomainDetails (Lude.Maybe Lude.Timestamp)
ddLastModifiedTime = Lens.lens (lastModifiedTime :: DomainDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DomainDetails)
{-# DEPRECATED ddLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainName :: Lens.Lens' DomainDetails (Lude.Maybe Lude.Text)
ddDomainName = Lens.lens (domainName :: DomainDetails -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: DomainDetails)
{-# DEPRECATED ddDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDomainId :: Lens.Lens' DomainDetails (Lude.Maybe Lude.Text)
ddDomainId = Lens.lens (domainId :: DomainDetails -> Lude.Maybe Lude.Text) (\s a -> s {domainId = a} :: DomainDetails)
{-# DEPRECATED ddDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.FromJSON DomainDetails where
  parseJSON =
    Lude.withObject
      "DomainDetails"
      ( \x ->
          DomainDetails'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "DomainArn")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "DomainName")
            Lude.<*> (x Lude..:? "DomainId")
      )
