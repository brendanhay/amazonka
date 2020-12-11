-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.CaptionSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.CaptionSource
  ( CaptionSource (..),

    -- * Smart constructor
    mkCaptionSource,

    -- * Lenses
    csTimeOffset,
    csEncryption,
    csKey,
    csLanguage,
    csLabel,
  )
where

import Network.AWS.ElasticTranscoder.Types.Encryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A source file for the input sidecar captions used during the transcoding process.
--
-- /See:/ 'mkCaptionSource' smart constructor.
data CaptionSource = CaptionSource'
  { timeOffset ::
      Lude.Maybe Lude.Text,
    encryption :: Lude.Maybe Encryption,
    key :: Lude.Maybe Lude.Text,
    language :: Lude.Maybe Lude.Text,
    label :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CaptionSource' with the minimum fields required to make a request.
--
-- * 'encryption' - The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
-- * 'key' - The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
-- * 'label' - The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
-- * 'language' - A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:
--
--
--     * 2-character ISO 639-1 code
--
--
--     * 3-character ISO 639-2 code
--
--
-- For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
-- * 'timeOffset' - For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions.
--
-- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
mkCaptionSource ::
  CaptionSource
mkCaptionSource =
  CaptionSource'
    { timeOffset = Lude.Nothing,
      encryption = Lude.Nothing,
      key = Lude.Nothing,
      language = Lude.Nothing,
      label = Lude.Nothing
    }

-- | For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions.
--
-- Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
--
-- /Note:/ Consider using 'timeOffset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTimeOffset :: Lens.Lens' CaptionSource (Lude.Maybe Lude.Text)
csTimeOffset = Lens.lens (timeOffset :: CaptionSource -> Lude.Maybe Lude.Text) (\s a -> s {timeOffset = a} :: CaptionSource)
{-# DEPRECATED csTimeOffset "Use generic-lens or generic-optics with 'timeOffset' instead." #-}

-- | The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csEncryption :: Lens.Lens' CaptionSource (Lude.Maybe Encryption)
csEncryption = Lens.lens (encryption :: CaptionSource -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: CaptionSource)
{-# DEPRECATED csEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csKey :: Lens.Lens' CaptionSource (Lude.Maybe Lude.Text)
csKey = Lens.lens (key :: CaptionSource -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: CaptionSource)
{-# DEPRECATED csKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:
--
--
--     * 2-character ISO 639-1 code
--
--
--     * 3-character ISO 639-2 code
--
--
-- For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
--
-- /Note:/ Consider using 'language' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLanguage :: Lens.Lens' CaptionSource (Lude.Maybe Lude.Text)
csLanguage = Lens.lens (language :: CaptionSource -> Lude.Maybe Lude.Text) (\s a -> s {language = a} :: CaptionSource)
{-# DEPRECATED csLanguage "Use generic-lens or generic-optics with 'language' instead." #-}

-- | The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csLabel :: Lens.Lens' CaptionSource (Lude.Maybe Lude.Text)
csLabel = Lens.lens (label :: CaptionSource -> Lude.Maybe Lude.Text) (\s a -> s {label = a} :: CaptionSource)
{-# DEPRECATED csLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.FromJSON CaptionSource where
  parseJSON =
    Lude.withObject
      "CaptionSource"
      ( \x ->
          CaptionSource'
            Lude.<$> (x Lude..:? "TimeOffset")
            Lude.<*> (x Lude..:? "Encryption")
            Lude.<*> (x Lude..:? "Key")
            Lude.<*> (x Lude..:? "Language")
            Lude.<*> (x Lude..:? "Label")
      )

instance Lude.ToJSON CaptionSource where
  toJSON CaptionSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TimeOffset" Lude..=) Lude.<$> timeOffset,
            ("Encryption" Lude..=) Lude.<$> encryption,
            ("Key" Lude..=) Lude.<$> key,
            ("Language" Lude..=) Lude.<$> language,
            ("Label" Lude..=) Lude.<$> label
          ]
      )
