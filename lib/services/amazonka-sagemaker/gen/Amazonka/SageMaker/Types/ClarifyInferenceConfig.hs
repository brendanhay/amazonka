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
-- Module      : Amazonka.SageMaker.Types.ClarifyInferenceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ClarifyInferenceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ClarifyFeatureType

-- | The inference configuration parameter for the model container.
--
-- /See:/ 'newClarifyInferenceConfig' smart constructor.
data ClarifyInferenceConfig = ClarifyInferenceConfig'
  { -- | A template string used to format a JSON record into an acceptable model
    -- container input. For example, a @ContentTemplate@ string
    -- @\'{\"myfeatures\":$features}\'@ will format a list of features
    -- @[1,2,3]@ into the record string @\'{\"myfeatures\":[1,2,3]}\'@.
    -- Required only when the model container input is in JSON Lines format.
    contentTemplate :: Prelude.Maybe Prelude.Text,
    -- | The names of the features. If provided, these are included in the
    -- endpoint response payload to help readability of the @InvokeEndpoint@
    -- output. See the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response Response>
    -- section under __Invoke the endpoint__ in the Developer Guide for more
    -- information.
    featureHeaders :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of data types of the features (optional). Applicable only to NLP
    -- explainability. If provided, @FeatureTypes@ must have at least one
    -- @\'text\'@ string (for example, @[\'text\']@). If @FeatureTypes@ is not
    -- provided, the explainer infers the feature types based on the baseline
    -- data. The feature types are included in the endpoint response payload.
    -- For additional information see the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response response>
    -- section under __Invoke the endpoint__ in the Developer Guide for more
    -- information.
    featureTypes :: Prelude.Maybe (Prelude.NonEmpty ClarifyFeatureType),
    -- | Provides the JMESPath expression to extract the features from a model
    -- container input in JSON Lines format. For example, if
    -- @FeaturesAttribute@ is the JMESPath expression @\'myfeatures\'@, it
    -- extracts a list of features @[1,2,3]@ from request data
    -- @\'{\"myfeatures\":[1,2,3]}\'@.
    featuresAttribute :: Prelude.Maybe Prelude.Text,
    -- | A JMESPath expression used to locate the list of label headers in the
    -- model container output.
    --
    -- __Example__: If the model container output of a batch request is
    -- @\'{\"labels\":[\"cat\",\"dog\",\"fish\"],\"probability\":[0.6,0.3,0.1]}\'@,
    -- then set @LabelAttribute@ to @\'labels\'@ to extract the list of label
    -- headers @[\"cat\",\"dog\",\"fish\"]@
    labelAttribute :: Prelude.Maybe Prelude.Text,
    -- | For multiclass classification problems, the label headers are the names
    -- of the classes. Otherwise, the label header is the name of the predicted
    -- label. These are used to help readability for the output of the
    -- @InvokeEndpoint@ API. See the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response response>
    -- section under __Invoke the endpoint__ in the Developer Guide for more
    -- information. If there are no label headers in the model container
    -- output, provide them manually using this parameter.
    labelHeaders :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A zero-based index used to extract a label header or list of label
    -- headers from model container output in CSV format.
    --
    -- __Example for a multiclass model:__ If the model container output
    -- consists of label headers followed by probabilities:
    -- @\'\"[\\\'cat\\\',\\\'dog\\\',\\\'fish\\\']\",\"[0.1,0.6,0.3]\"\'@, set
    -- @LabelIndex@ to @0@ to select the label headers
    -- @[\'cat\',\'dog\',\'fish\']@.
    labelIndex :: Prelude.Maybe Prelude.Natural,
    -- | The maximum payload size (MB) allowed of a request from the explainer to
    -- the model container. Defaults to @6@ MB.
    maxPayloadInMB :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of records in a request that the model container can
    -- process when querying the model container for the predictions of a
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-synthetic synthetic dataset>.
    -- A record is a unit of input data that inference can be made on, for
    -- example, a single line in CSV data. If @MaxRecordCount@ is @1@, the
    -- model container expects one record per request. A value of 2 or greater
    -- means that the model expects batch requests, which can reduce overhead
    -- and speed up the inferencing process. If this parameter is not provided,
    -- the explainer will tune the record count per request according to the
    -- model container\'s capacity at runtime.
    maxRecordCount :: Prelude.Maybe Prelude.Natural,
    -- | A JMESPath expression used to extract the probability (or score) from
    -- the model container output if the model container is in JSON Lines
    -- format.
    --
    -- __Example__: If the model container output of a single request is
    -- @\'{\"predicted_label\":1,\"probability\":0.6}\'@, then set
    -- @ProbabilityAttribute@ to @\'probability\'@.
    probabilityAttribute :: Prelude.Maybe Prelude.Text,
    -- | A zero-based index used to extract a probability value (score) or list
    -- from model container output in CSV format. If this value is not
    -- provided, the entire model container output will be treated as a
    -- probability value (score) or list.
    --
    -- __Example for a single class model:__ If the model container output
    -- consists of a string-formatted prediction label followed by its
    -- probability: @\'1,0.6\'@, set @ProbabilityIndex@ to @1@ to select the
    -- probability value @0.6@.
    --
    -- __Example for a multiclass model:__ If the model container output
    -- consists of a string-formatted prediction label followed by its
    -- probability:
    -- @\'\"[\\\'cat\\\',\\\'dog\\\',\\\'fish\\\']\",\"[0.1,0.6,0.3]\"\'@, set
    -- @ProbabilityIndex@ to @1@ to select the probability values
    -- @[0.1,0.6,0.3]@.
    probabilityIndex :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClarifyInferenceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentTemplate', 'clarifyInferenceConfig_contentTemplate' - A template string used to format a JSON record into an acceptable model
-- container input. For example, a @ContentTemplate@ string
-- @\'{\"myfeatures\":$features}\'@ will format a list of features
-- @[1,2,3]@ into the record string @\'{\"myfeatures\":[1,2,3]}\'@.
-- Required only when the model container input is in JSON Lines format.
--
-- 'featureHeaders', 'clarifyInferenceConfig_featureHeaders' - The names of the features. If provided, these are included in the
-- endpoint response payload to help readability of the @InvokeEndpoint@
-- output. See the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response Response>
-- section under __Invoke the endpoint__ in the Developer Guide for more
-- information.
--
-- 'featureTypes', 'clarifyInferenceConfig_featureTypes' - A list of data types of the features (optional). Applicable only to NLP
-- explainability. If provided, @FeatureTypes@ must have at least one
-- @\'text\'@ string (for example, @[\'text\']@). If @FeatureTypes@ is not
-- provided, the explainer infers the feature types based on the baseline
-- data. The feature types are included in the endpoint response payload.
-- For additional information see the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response response>
-- section under __Invoke the endpoint__ in the Developer Guide for more
-- information.
--
-- 'featuresAttribute', 'clarifyInferenceConfig_featuresAttribute' - Provides the JMESPath expression to extract the features from a model
-- container input in JSON Lines format. For example, if
-- @FeaturesAttribute@ is the JMESPath expression @\'myfeatures\'@, it
-- extracts a list of features @[1,2,3]@ from request data
-- @\'{\"myfeatures\":[1,2,3]}\'@.
--
-- 'labelAttribute', 'clarifyInferenceConfig_labelAttribute' - A JMESPath expression used to locate the list of label headers in the
-- model container output.
--
-- __Example__: If the model container output of a batch request is
-- @\'{\"labels\":[\"cat\",\"dog\",\"fish\"],\"probability\":[0.6,0.3,0.1]}\'@,
-- then set @LabelAttribute@ to @\'labels\'@ to extract the list of label
-- headers @[\"cat\",\"dog\",\"fish\"]@
--
-- 'labelHeaders', 'clarifyInferenceConfig_labelHeaders' - For multiclass classification problems, the label headers are the names
-- of the classes. Otherwise, the label header is the name of the predicted
-- label. These are used to help readability for the output of the
-- @InvokeEndpoint@ API. See the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response response>
-- section under __Invoke the endpoint__ in the Developer Guide for more
-- information. If there are no label headers in the model container
-- output, provide them manually using this parameter.
--
-- 'labelIndex', 'clarifyInferenceConfig_labelIndex' - A zero-based index used to extract a label header or list of label
-- headers from model container output in CSV format.
--
-- __Example for a multiclass model:__ If the model container output
-- consists of label headers followed by probabilities:
-- @\'\"[\\\'cat\\\',\\\'dog\\\',\\\'fish\\\']\",\"[0.1,0.6,0.3]\"\'@, set
-- @LabelIndex@ to @0@ to select the label headers
-- @[\'cat\',\'dog\',\'fish\']@.
--
-- 'maxPayloadInMB', 'clarifyInferenceConfig_maxPayloadInMB' - The maximum payload size (MB) allowed of a request from the explainer to
-- the model container. Defaults to @6@ MB.
--
-- 'maxRecordCount', 'clarifyInferenceConfig_maxRecordCount' - The maximum number of records in a request that the model container can
-- process when querying the model container for the predictions of a
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-synthetic synthetic dataset>.
-- A record is a unit of input data that inference can be made on, for
-- example, a single line in CSV data. If @MaxRecordCount@ is @1@, the
-- model container expects one record per request. A value of 2 or greater
-- means that the model expects batch requests, which can reduce overhead
-- and speed up the inferencing process. If this parameter is not provided,
-- the explainer will tune the record count per request according to the
-- model container\'s capacity at runtime.
--
-- 'probabilityAttribute', 'clarifyInferenceConfig_probabilityAttribute' - A JMESPath expression used to extract the probability (or score) from
-- the model container output if the model container is in JSON Lines
-- format.
--
-- __Example__: If the model container output of a single request is
-- @\'{\"predicted_label\":1,\"probability\":0.6}\'@, then set
-- @ProbabilityAttribute@ to @\'probability\'@.
--
-- 'probabilityIndex', 'clarifyInferenceConfig_probabilityIndex' - A zero-based index used to extract a probability value (score) or list
-- from model container output in CSV format. If this value is not
-- provided, the entire model container output will be treated as a
-- probability value (score) or list.
--
-- __Example for a single class model:__ If the model container output
-- consists of a string-formatted prediction label followed by its
-- probability: @\'1,0.6\'@, set @ProbabilityIndex@ to @1@ to select the
-- probability value @0.6@.
--
-- __Example for a multiclass model:__ If the model container output
-- consists of a string-formatted prediction label followed by its
-- probability:
-- @\'\"[\\\'cat\\\',\\\'dog\\\',\\\'fish\\\']\",\"[0.1,0.6,0.3]\"\'@, set
-- @ProbabilityIndex@ to @1@ to select the probability values
-- @[0.1,0.6,0.3]@.
newClarifyInferenceConfig ::
  ClarifyInferenceConfig
newClarifyInferenceConfig =
  ClarifyInferenceConfig'
    { contentTemplate =
        Prelude.Nothing,
      featureHeaders = Prelude.Nothing,
      featureTypes = Prelude.Nothing,
      featuresAttribute = Prelude.Nothing,
      labelAttribute = Prelude.Nothing,
      labelHeaders = Prelude.Nothing,
      labelIndex = Prelude.Nothing,
      maxPayloadInMB = Prelude.Nothing,
      maxRecordCount = Prelude.Nothing,
      probabilityAttribute = Prelude.Nothing,
      probabilityIndex = Prelude.Nothing
    }

-- | A template string used to format a JSON record into an acceptable model
-- container input. For example, a @ContentTemplate@ string
-- @\'{\"myfeatures\":$features}\'@ will format a list of features
-- @[1,2,3]@ into the record string @\'{\"myfeatures\":[1,2,3]}\'@.
-- Required only when the model container input is in JSON Lines format.
clarifyInferenceConfig_contentTemplate :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Text)
clarifyInferenceConfig_contentTemplate = Lens.lens (\ClarifyInferenceConfig' {contentTemplate} -> contentTemplate) (\s@ClarifyInferenceConfig' {} a -> s {contentTemplate = a} :: ClarifyInferenceConfig)

-- | The names of the features. If provided, these are included in the
-- endpoint response payload to help readability of the @InvokeEndpoint@
-- output. See the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response Response>
-- section under __Invoke the endpoint__ in the Developer Guide for more
-- information.
clarifyInferenceConfig_featureHeaders :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
clarifyInferenceConfig_featureHeaders = Lens.lens (\ClarifyInferenceConfig' {featureHeaders} -> featureHeaders) (\s@ClarifyInferenceConfig' {} a -> s {featureHeaders = a} :: ClarifyInferenceConfig) Prelude.. Lens.mapping Lens.coerced

-- | A list of data types of the features (optional). Applicable only to NLP
-- explainability. If provided, @FeatureTypes@ must have at least one
-- @\'text\'@ string (for example, @[\'text\']@). If @FeatureTypes@ is not
-- provided, the explainer infers the feature types based on the baseline
-- data. The feature types are included in the endpoint response payload.
-- For additional information see the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response response>
-- section under __Invoke the endpoint__ in the Developer Guide for more
-- information.
clarifyInferenceConfig_featureTypes :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe (Prelude.NonEmpty ClarifyFeatureType))
clarifyInferenceConfig_featureTypes = Lens.lens (\ClarifyInferenceConfig' {featureTypes} -> featureTypes) (\s@ClarifyInferenceConfig' {} a -> s {featureTypes = a} :: ClarifyInferenceConfig) Prelude.. Lens.mapping Lens.coerced

-- | Provides the JMESPath expression to extract the features from a model
-- container input in JSON Lines format. For example, if
-- @FeaturesAttribute@ is the JMESPath expression @\'myfeatures\'@, it
-- extracts a list of features @[1,2,3]@ from request data
-- @\'{\"myfeatures\":[1,2,3]}\'@.
clarifyInferenceConfig_featuresAttribute :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Text)
clarifyInferenceConfig_featuresAttribute = Lens.lens (\ClarifyInferenceConfig' {featuresAttribute} -> featuresAttribute) (\s@ClarifyInferenceConfig' {} a -> s {featuresAttribute = a} :: ClarifyInferenceConfig)

-- | A JMESPath expression used to locate the list of label headers in the
-- model container output.
--
-- __Example__: If the model container output of a batch request is
-- @\'{\"labels\":[\"cat\",\"dog\",\"fish\"],\"probability\":[0.6,0.3,0.1]}\'@,
-- then set @LabelAttribute@ to @\'labels\'@ to extract the list of label
-- headers @[\"cat\",\"dog\",\"fish\"]@
clarifyInferenceConfig_labelAttribute :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Text)
clarifyInferenceConfig_labelAttribute = Lens.lens (\ClarifyInferenceConfig' {labelAttribute} -> labelAttribute) (\s@ClarifyInferenceConfig' {} a -> s {labelAttribute = a} :: ClarifyInferenceConfig)

-- | For multiclass classification problems, the label headers are the names
-- of the classes. Otherwise, the label header is the name of the predicted
-- label. These are used to help readability for the output of the
-- @InvokeEndpoint@ API. See the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-invoke-endpoint.html#clarify-online-explainability-response response>
-- section under __Invoke the endpoint__ in the Developer Guide for more
-- information. If there are no label headers in the model container
-- output, provide them manually using this parameter.
clarifyInferenceConfig_labelHeaders :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
clarifyInferenceConfig_labelHeaders = Lens.lens (\ClarifyInferenceConfig' {labelHeaders} -> labelHeaders) (\s@ClarifyInferenceConfig' {} a -> s {labelHeaders = a} :: ClarifyInferenceConfig) Prelude.. Lens.mapping Lens.coerced

-- | A zero-based index used to extract a label header or list of label
-- headers from model container output in CSV format.
--
-- __Example for a multiclass model:__ If the model container output
-- consists of label headers followed by probabilities:
-- @\'\"[\\\'cat\\\',\\\'dog\\\',\\\'fish\\\']\",\"[0.1,0.6,0.3]\"\'@, set
-- @LabelIndex@ to @0@ to select the label headers
-- @[\'cat\',\'dog\',\'fish\']@.
clarifyInferenceConfig_labelIndex :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Natural)
clarifyInferenceConfig_labelIndex = Lens.lens (\ClarifyInferenceConfig' {labelIndex} -> labelIndex) (\s@ClarifyInferenceConfig' {} a -> s {labelIndex = a} :: ClarifyInferenceConfig)

-- | The maximum payload size (MB) allowed of a request from the explainer to
-- the model container. Defaults to @6@ MB.
clarifyInferenceConfig_maxPayloadInMB :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Natural)
clarifyInferenceConfig_maxPayloadInMB = Lens.lens (\ClarifyInferenceConfig' {maxPayloadInMB} -> maxPayloadInMB) (\s@ClarifyInferenceConfig' {} a -> s {maxPayloadInMB = a} :: ClarifyInferenceConfig)

-- | The maximum number of records in a request that the model container can
-- process when querying the model container for the predictions of a
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/clarify-online-explainability-create-endpoint.html#clarify-online-explainability-create-endpoint-synthetic synthetic dataset>.
-- A record is a unit of input data that inference can be made on, for
-- example, a single line in CSV data. If @MaxRecordCount@ is @1@, the
-- model container expects one record per request. A value of 2 or greater
-- means that the model expects batch requests, which can reduce overhead
-- and speed up the inferencing process. If this parameter is not provided,
-- the explainer will tune the record count per request according to the
-- model container\'s capacity at runtime.
clarifyInferenceConfig_maxRecordCount :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Natural)
clarifyInferenceConfig_maxRecordCount = Lens.lens (\ClarifyInferenceConfig' {maxRecordCount} -> maxRecordCount) (\s@ClarifyInferenceConfig' {} a -> s {maxRecordCount = a} :: ClarifyInferenceConfig)

-- | A JMESPath expression used to extract the probability (or score) from
-- the model container output if the model container is in JSON Lines
-- format.
--
-- __Example__: If the model container output of a single request is
-- @\'{\"predicted_label\":1,\"probability\":0.6}\'@, then set
-- @ProbabilityAttribute@ to @\'probability\'@.
clarifyInferenceConfig_probabilityAttribute :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Text)
clarifyInferenceConfig_probabilityAttribute = Lens.lens (\ClarifyInferenceConfig' {probabilityAttribute} -> probabilityAttribute) (\s@ClarifyInferenceConfig' {} a -> s {probabilityAttribute = a} :: ClarifyInferenceConfig)

-- | A zero-based index used to extract a probability value (score) or list
-- from model container output in CSV format. If this value is not
-- provided, the entire model container output will be treated as a
-- probability value (score) or list.
--
-- __Example for a single class model:__ If the model container output
-- consists of a string-formatted prediction label followed by its
-- probability: @\'1,0.6\'@, set @ProbabilityIndex@ to @1@ to select the
-- probability value @0.6@.
--
-- __Example for a multiclass model:__ If the model container output
-- consists of a string-formatted prediction label followed by its
-- probability:
-- @\'\"[\\\'cat\\\',\\\'dog\\\',\\\'fish\\\']\",\"[0.1,0.6,0.3]\"\'@, set
-- @ProbabilityIndex@ to @1@ to select the probability values
-- @[0.1,0.6,0.3]@.
clarifyInferenceConfig_probabilityIndex :: Lens.Lens' ClarifyInferenceConfig (Prelude.Maybe Prelude.Natural)
clarifyInferenceConfig_probabilityIndex = Lens.lens (\ClarifyInferenceConfig' {probabilityIndex} -> probabilityIndex) (\s@ClarifyInferenceConfig' {} a -> s {probabilityIndex = a} :: ClarifyInferenceConfig)

instance Data.FromJSON ClarifyInferenceConfig where
  parseJSON =
    Data.withObject
      "ClarifyInferenceConfig"
      ( \x ->
          ClarifyInferenceConfig'
            Prelude.<$> (x Data..:? "ContentTemplate")
            Prelude.<*> (x Data..:? "FeatureHeaders")
            Prelude.<*> (x Data..:? "FeatureTypes")
            Prelude.<*> (x Data..:? "FeaturesAttribute")
            Prelude.<*> (x Data..:? "LabelAttribute")
            Prelude.<*> (x Data..:? "LabelHeaders")
            Prelude.<*> (x Data..:? "LabelIndex")
            Prelude.<*> (x Data..:? "MaxPayloadInMB")
            Prelude.<*> (x Data..:? "MaxRecordCount")
            Prelude.<*> (x Data..:? "ProbabilityAttribute")
            Prelude.<*> (x Data..:? "ProbabilityIndex")
      )

instance Prelude.Hashable ClarifyInferenceConfig where
  hashWithSalt _salt ClarifyInferenceConfig' {..} =
    _salt `Prelude.hashWithSalt` contentTemplate
      `Prelude.hashWithSalt` featureHeaders
      `Prelude.hashWithSalt` featureTypes
      `Prelude.hashWithSalt` featuresAttribute
      `Prelude.hashWithSalt` labelAttribute
      `Prelude.hashWithSalt` labelHeaders
      `Prelude.hashWithSalt` labelIndex
      `Prelude.hashWithSalt` maxPayloadInMB
      `Prelude.hashWithSalt` maxRecordCount
      `Prelude.hashWithSalt` probabilityAttribute
      `Prelude.hashWithSalt` probabilityIndex

instance Prelude.NFData ClarifyInferenceConfig where
  rnf ClarifyInferenceConfig' {..} =
    Prelude.rnf contentTemplate
      `Prelude.seq` Prelude.rnf featureHeaders
      `Prelude.seq` Prelude.rnf featureTypes
      `Prelude.seq` Prelude.rnf featuresAttribute
      `Prelude.seq` Prelude.rnf labelAttribute
      `Prelude.seq` Prelude.rnf labelHeaders
      `Prelude.seq` Prelude.rnf labelIndex
      `Prelude.seq` Prelude.rnf maxPayloadInMB
      `Prelude.seq` Prelude.rnf maxRecordCount
      `Prelude.seq` Prelude.rnf probabilityAttribute
      `Prelude.seq` Prelude.rnf probabilityIndex

instance Data.ToJSON ClarifyInferenceConfig where
  toJSON ClarifyInferenceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentTemplate" Data..=)
              Prelude.<$> contentTemplate,
            ("FeatureHeaders" Data..=)
              Prelude.<$> featureHeaders,
            ("FeatureTypes" Data..=) Prelude.<$> featureTypes,
            ("FeaturesAttribute" Data..=)
              Prelude.<$> featuresAttribute,
            ("LabelAttribute" Data..=)
              Prelude.<$> labelAttribute,
            ("LabelHeaders" Data..=) Prelude.<$> labelHeaders,
            ("LabelIndex" Data..=) Prelude.<$> labelIndex,
            ("MaxPayloadInMB" Data..=)
              Prelude.<$> maxPayloadInMB,
            ("MaxRecordCount" Data..=)
              Prelude.<$> maxRecordCount,
            ("ProbabilityAttribute" Data..=)
              Prelude.<$> probabilityAttribute,
            ("ProbabilityIndex" Data..=)
              Prelude.<$> probabilityIndex
          ]
      )
