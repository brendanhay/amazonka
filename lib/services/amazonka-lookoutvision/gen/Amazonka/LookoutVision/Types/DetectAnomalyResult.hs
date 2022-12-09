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
-- Module      : Amazonka.LookoutVision.Types.DetectAnomalyResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.DetectAnomalyResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.Anomaly
import Amazonka.LookoutVision.Types.ImageSource
import qualified Amazonka.Prelude as Prelude

-- | The prediction results from a call to DetectAnomalies.
-- @DetectAnomalyResult@ includes classification information for the
-- prediction (@IsAnomalous@ and @Confidence@). If the model you use is an
-- image segementation model, @DetectAnomalyResult@ also includes
-- segmentation information (@Anomalies@ and @AnomalyMask@). Classification
-- information is calculated separately from segmentation information and
-- you shouldn\'t assume a relationship between them.
--
-- /See:/ 'newDetectAnomalyResult' smart constructor.
data DetectAnomalyResult = DetectAnomalyResult'
  { -- | If the model is an image segmentation model, @Anomalies@ contains a list
    -- of anomaly types found in the image. There is one entry for each type of
    -- anomaly found (even if multiple instances of an anomaly type exist on
    -- the image). The first element in the list is always an anomaly type
    -- representing the image background (\'background\') and shouldn\'t be
    -- considered an anomaly. Amazon Lookout for Vision automatically add the
    -- background anomaly type to the response, and you don\'t need to declare
    -- a background anomaly type in your dataset.
    --
    -- If the list has one entry (\'background\'), no anomalies were found on
    -- the image.
    --
    -- An image classification model doesn\'t return an @Anomalies@ list.
    anomalies :: Prelude.Maybe [Anomaly],
    -- | If the model is an image segmentation model, @AnomalyMask@ contains
    -- pixel masks that covers all anomaly types found on the image. Each
    -- anomaly type has a different mask color. To map a color to an anomaly
    -- type, see the @color@ field of the PixelAnomaly object.
    --
    -- An image classification model doesn\'t return an @Anomalies@ list.
    anomalyMask :: Prelude.Maybe Data.Base64,
    -- | The confidence that Lookout for Vision has in the accuracy of the
    -- classification in @IsAnomalous@.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | True if Amazon Lookout for Vision classifies the image as containing an
    -- anomaly, otherwise false.
    isAnomalous :: Prelude.Maybe Prelude.Bool,
    -- | The source of the image that was analyzed. @direct@ means that the
    -- images was supplied from the local computer. No other values are
    -- supported.
    source :: Prelude.Maybe ImageSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectAnomalyResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalies', 'detectAnomalyResult_anomalies' - If the model is an image segmentation model, @Anomalies@ contains a list
-- of anomaly types found in the image. There is one entry for each type of
-- anomaly found (even if multiple instances of an anomaly type exist on
-- the image). The first element in the list is always an anomaly type
-- representing the image background (\'background\') and shouldn\'t be
-- considered an anomaly. Amazon Lookout for Vision automatically add the
-- background anomaly type to the response, and you don\'t need to declare
-- a background anomaly type in your dataset.
--
-- If the list has one entry (\'background\'), no anomalies were found on
-- the image.
--
-- An image classification model doesn\'t return an @Anomalies@ list.
--
-- 'anomalyMask', 'detectAnomalyResult_anomalyMask' - If the model is an image segmentation model, @AnomalyMask@ contains
-- pixel masks that covers all anomaly types found on the image. Each
-- anomaly type has a different mask color. To map a color to an anomaly
-- type, see the @color@ field of the PixelAnomaly object.
--
-- An image classification model doesn\'t return an @Anomalies@ list.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'confidence', 'detectAnomalyResult_confidence' - The confidence that Lookout for Vision has in the accuracy of the
-- classification in @IsAnomalous@.
--
-- 'isAnomalous', 'detectAnomalyResult_isAnomalous' - True if Amazon Lookout for Vision classifies the image as containing an
-- anomaly, otherwise false.
--
-- 'source', 'detectAnomalyResult_source' - The source of the image that was analyzed. @direct@ means that the
-- images was supplied from the local computer. No other values are
-- supported.
newDetectAnomalyResult ::
  DetectAnomalyResult
newDetectAnomalyResult =
  DetectAnomalyResult'
    { anomalies = Prelude.Nothing,
      anomalyMask = Prelude.Nothing,
      confidence = Prelude.Nothing,
      isAnomalous = Prelude.Nothing,
      source = Prelude.Nothing
    }

-- | If the model is an image segmentation model, @Anomalies@ contains a list
-- of anomaly types found in the image. There is one entry for each type of
-- anomaly found (even if multiple instances of an anomaly type exist on
-- the image). The first element in the list is always an anomaly type
-- representing the image background (\'background\') and shouldn\'t be
-- considered an anomaly. Amazon Lookout for Vision automatically add the
-- background anomaly type to the response, and you don\'t need to declare
-- a background anomaly type in your dataset.
--
-- If the list has one entry (\'background\'), no anomalies were found on
-- the image.
--
-- An image classification model doesn\'t return an @Anomalies@ list.
detectAnomalyResult_anomalies :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe [Anomaly])
detectAnomalyResult_anomalies = Lens.lens (\DetectAnomalyResult' {anomalies} -> anomalies) (\s@DetectAnomalyResult' {} a -> s {anomalies = a} :: DetectAnomalyResult) Prelude.. Lens.mapping Lens.coerced

-- | If the model is an image segmentation model, @AnomalyMask@ contains
-- pixel masks that covers all anomaly types found on the image. Each
-- anomaly type has a different mask color. To map a color to an anomaly
-- type, see the @color@ field of the PixelAnomaly object.
--
-- An image classification model doesn\'t return an @Anomalies@ list.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
detectAnomalyResult_anomalyMask :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe Prelude.ByteString)
detectAnomalyResult_anomalyMask = Lens.lens (\DetectAnomalyResult' {anomalyMask} -> anomalyMask) (\s@DetectAnomalyResult' {} a -> s {anomalyMask = a} :: DetectAnomalyResult) Prelude.. Lens.mapping Data._Base64

-- | The confidence that Lookout for Vision has in the accuracy of the
-- classification in @IsAnomalous@.
detectAnomalyResult_confidence :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe Prelude.Double)
detectAnomalyResult_confidence = Lens.lens (\DetectAnomalyResult' {confidence} -> confidence) (\s@DetectAnomalyResult' {} a -> s {confidence = a} :: DetectAnomalyResult)

-- | True if Amazon Lookout for Vision classifies the image as containing an
-- anomaly, otherwise false.
detectAnomalyResult_isAnomalous :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe Prelude.Bool)
detectAnomalyResult_isAnomalous = Lens.lens (\DetectAnomalyResult' {isAnomalous} -> isAnomalous) (\s@DetectAnomalyResult' {} a -> s {isAnomalous = a} :: DetectAnomalyResult)

-- | The source of the image that was analyzed. @direct@ means that the
-- images was supplied from the local computer. No other values are
-- supported.
detectAnomalyResult_source :: Lens.Lens' DetectAnomalyResult (Prelude.Maybe ImageSource)
detectAnomalyResult_source = Lens.lens (\DetectAnomalyResult' {source} -> source) (\s@DetectAnomalyResult' {} a -> s {source = a} :: DetectAnomalyResult)

instance Data.FromJSON DetectAnomalyResult where
  parseJSON =
    Data.withObject
      "DetectAnomalyResult"
      ( \x ->
          DetectAnomalyResult'
            Prelude.<$> (x Data..:? "Anomalies" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AnomalyMask")
            Prelude.<*> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "IsAnomalous")
            Prelude.<*> (x Data..:? "Source")
      )

instance Prelude.Hashable DetectAnomalyResult where
  hashWithSalt _salt DetectAnomalyResult' {..} =
    _salt `Prelude.hashWithSalt` anomalies
      `Prelude.hashWithSalt` anomalyMask
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` isAnomalous
      `Prelude.hashWithSalt` source

instance Prelude.NFData DetectAnomalyResult where
  rnf DetectAnomalyResult' {..} =
    Prelude.rnf anomalies
      `Prelude.seq` Prelude.rnf anomalyMask
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf isAnomalous
      `Prelude.seq` Prelude.rnf source
