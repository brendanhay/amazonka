{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessor
  ( StreamProcessor (..),

    -- * Smart constructor
    mkStreamProcessor,

    -- * Lenses
    spStatus,
    spName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.StreamProcessorStatus

-- | An object that recognizes faces in a streaming video. An Amazon Rekognition stream processor is created by a call to 'CreateStreamProcessor' . The request parameters for @CreateStreamProcessor@ describe the Kinesis video stream source for the streaming video, face recognition parameters, and where to stream the analysis resullts.
--
-- /See:/ 'mkStreamProcessor' smart constructor.
data StreamProcessor = StreamProcessor'
  { -- | Current status of the Amazon Rekognition stream processor.
    status :: Lude.Maybe StreamProcessorStatus,
    -- | Name of the Amazon Rekognition stream processor.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamProcessor' with the minimum fields required to make a request.
--
-- * 'status' - Current status of the Amazon Rekognition stream processor.
-- * 'name' - Name of the Amazon Rekognition stream processor.
mkStreamProcessor ::
  StreamProcessor
mkStreamProcessor =
  StreamProcessor' {status = Lude.Nothing, name = Lude.Nothing}

-- | Current status of the Amazon Rekognition stream processor.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spStatus :: Lens.Lens' StreamProcessor (Lude.Maybe StreamProcessorStatus)
spStatus = Lens.lens (status :: StreamProcessor -> Lude.Maybe StreamProcessorStatus) (\s a -> s {status = a} :: StreamProcessor)
{-# DEPRECATED spStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Name of the Amazon Rekognition stream processor.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' StreamProcessor (Lude.Maybe Lude.Text)
spName = Lens.lens (name :: StreamProcessor -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StreamProcessor)
{-# DEPRECATED spName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON StreamProcessor where
  parseJSON =
    Lude.withObject
      "StreamProcessor"
      ( \x ->
          StreamProcessor'
            Lude.<$> (x Lude..:? "Status") Lude.<*> (x Lude..:? "Name")
      )
