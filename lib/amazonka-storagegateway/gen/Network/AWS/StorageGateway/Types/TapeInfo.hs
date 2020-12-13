{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeInfo
  ( TapeInfo (..),

    -- * Smart constructor
    mkTapeInfo,

    -- * Lenses
    tiTapeBarcode,
    tiTapeStatus,
    tiTapeARN,
    tiGatewayARN,
    tiTapeSizeInBytes,
    tiPoolId,
    tiPoolEntryDate,
    tiRetentionStartDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a virtual tape.
--
-- /See:/ 'mkTapeInfo' smart constructor.
data TapeInfo = TapeInfo'
  { -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Lude.Maybe Lude.Text,
    -- | The status of the tape.
    tapeStatus :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of a virtual tape.
    tapeARN :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Lude.Maybe Lude.Text,
    -- | The size, in bytes, of a virtual tape.
    tapeSizeInBytes :: Lude.Maybe Lude.Integer,
    -- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Lude.Maybe Lude.Text,
    -- | The date that the tape entered the custom tape pool with tape retention lock enabled.
    poolEntryDate :: Lude.Maybe Lude.Timestamp,
    -- | The date that the tape became subject to tape retention lock.
    retentionStartDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TapeInfo' with the minimum fields required to make a request.
--
-- * 'tapeBarcode' - The barcode that identifies a specific virtual tape.
-- * 'tapeStatus' - The status of the tape.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of a virtual tape.
-- * 'gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
-- * 'tapeSizeInBytes' - The size, in bytes, of a virtual tape.
-- * 'poolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
-- * 'poolEntryDate' - The date that the tape entered the custom tape pool with tape retention lock enabled.
-- * 'retentionStartDate' - The date that the tape became subject to tape retention lock.
mkTapeInfo ::
  TapeInfo
mkTapeInfo =
  TapeInfo'
    { tapeBarcode = Lude.Nothing,
      tapeStatus = Lude.Nothing,
      tapeARN = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      tapeSizeInBytes = Lude.Nothing,
      poolId = Lude.Nothing,
      poolEntryDate = Lude.Nothing,
      retentionStartDate = Lude.Nothing
    }

-- | The barcode that identifies a specific virtual tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeBarcode :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Text)
tiTapeBarcode = Lens.lens (tapeBarcode :: TapeInfo -> Lude.Maybe Lude.Text) (\s a -> s {tapeBarcode = a} :: TapeInfo)
{-# DEPRECATED tiTapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead." #-}

-- | The status of the tape.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeStatus :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Text)
tiTapeStatus = Lens.lens (tapeStatus :: TapeInfo -> Lude.Maybe Lude.Text) (\s a -> s {tapeStatus = a} :: TapeInfo)
{-# DEPRECATED tiTapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of a virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeARN :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Text)
tiTapeARN = Lens.lens (tapeARN :: TapeInfo -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: TapeInfo)
{-# DEPRECATED tiTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiGatewayARN :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Text)
tiGatewayARN = Lens.lens (gatewayARN :: TapeInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: TapeInfo)
{-# DEPRECATED tiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The size, in bytes, of a virtual tape.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeSizeInBytes :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Integer)
tiTapeSizeInBytes = Lens.lens (tapeSizeInBytes :: TapeInfo -> Lude.Maybe Lude.Integer) (\s a -> s {tapeSizeInBytes = a} :: TapeInfo)
{-# DEPRECATED tiTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiPoolId :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Text)
tiPoolId = Lens.lens (poolId :: TapeInfo -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: TapeInfo)
{-# DEPRECATED tiPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | The date that the tape entered the custom tape pool with tape retention lock enabled.
--
-- /Note:/ Consider using 'poolEntryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiPoolEntryDate :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Timestamp)
tiPoolEntryDate = Lens.lens (poolEntryDate :: TapeInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {poolEntryDate = a} :: TapeInfo)
{-# DEPRECATED tiPoolEntryDate "Use generic-lens or generic-optics with 'poolEntryDate' instead." #-}

-- | The date that the tape became subject to tape retention lock.
--
-- /Note:/ Consider using 'retentionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiRetentionStartDate :: Lens.Lens' TapeInfo (Lude.Maybe Lude.Timestamp)
tiRetentionStartDate = Lens.lens (retentionStartDate :: TapeInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {retentionStartDate = a} :: TapeInfo)
{-# DEPRECATED tiRetentionStartDate "Use generic-lens or generic-optics with 'retentionStartDate' instead." #-}

instance Lude.FromJSON TapeInfo where
  parseJSON =
    Lude.withObject
      "TapeInfo"
      ( \x ->
          TapeInfo'
            Lude.<$> (x Lude..:? "TapeBarcode")
            Lude.<*> (x Lude..:? "TapeStatus")
            Lude.<*> (x Lude..:? "TapeARN")
            Lude.<*> (x Lude..:? "GatewayARN")
            Lude.<*> (x Lude..:? "TapeSizeInBytes")
            Lude.<*> (x Lude..:? "PoolId")
            Lude.<*> (x Lude..:? "PoolEntryDate")
            Lude.<*> (x Lude..:? "RetentionStartDate")
      )
