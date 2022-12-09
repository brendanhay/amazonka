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
-- Module      : Amazonka.Rum.Types.MetricDefinitionRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.MetricDefinitionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to define one extended metric that RUM will send to
-- CloudWatch or CloudWatch Evidently. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-RUM-vended-metrics.html Additional metrics that you can send to CloudWatch and CloudWatch Evidently>.
--
-- Only certain combinations of values for @Name@, @ValueKey@, and
-- @EventPattern@ are valid. In addition to what is displayed in the list
-- below, the @EventPattern@ can also include information used by the
-- @DimensionKeys@ field.
--
-- -   If @Name@ is @PerformanceNavigationDuration@, then @ValueKey@must be
--     @event_details.duration@ and the @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.performance_navigation_event\"]}@
--
-- -   If @Name@ is @PerformanceResourceDuration@, then @ValueKey@must be
--     @event_details.duration@ and the @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.performance_resource_event\"]}@
--
-- -   If @Name@ is @NavigationSatisfiedTransaction@, then @ValueKey@must
--     be null and the @EventPattern@ must include
--     @{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"event_details\": { \"duration\": [{ \"numeric\": [\">\",2000] }] } }@
--
-- -   If @Name@ is @NavigationToleratedTransaction@, then @ValueKey@must
--     be null and the @EventPattern@ must include
--     @{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"event_details\": { \"duration\": [{ \"numeric\": [\">=\",2000,\"\<\"8000] }] } }@
--
-- -   If @Name@ is @NavigationFrustratedTransaction@, then @ValueKey@must
--     be null and the @EventPattern@ must include
--     @{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"event_details\": { \"duration\": [{ \"numeric\": [\">=\",8000] }] } }@
--
-- -   If @Name@ is @WebVitalsCumulativeLayoutShift@, then @ValueKey@must
--     be @event_details.value@ and the @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.cumulative_layout_shift_event\"]}@
--
-- -   If @Name@ is @WebVitalsFirstInputDelay@, then @ValueKey@must be
--     @event_details.value@ and the @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.first_input_delay_event\"]}@
--
-- -   If @Name@ is @WebVitalsLargestContentfulPaint@, then @ValueKey@must
--     be @event_details.value@ and the @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.largest_contentful_paint_event\"]}@
--
-- -   If @Name@ is @JsErrorCount@, then @ValueKey@must be null and the
--     @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.js_error_event\"]}@
--
-- -   If @Name@ is @HttpErrorCount@, then @ValueKey@must be null and the
--     @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.http_event\"]}@
--
-- -   If @Name@ is @SessionCount@, then @ValueKey@must be null and the
--     @EventPattern@ must include
--     @{\"event_type\":[\"com.amazon.rum.session_start_event\"]}@
--
-- /See:/ 'newMetricDefinitionRequest' smart constructor.
data MetricDefinitionRequest = MetricDefinitionRequest'
  { -- | Use this field only if you are sending the metric to CloudWatch.
    --
    -- This field is a map of field paths to dimension names. It defines the
    -- dimensions to associate with this metric in CloudWatch. Valid values for
    -- the entries in this field are the following:
    --
    -- -   @\"metadata.pageId\": \"PageId\"@
    --
    -- -   @\"metadata.browserName\": \"BrowserName\"@
    --
    -- -   @\"metadata.deviceType\": \"DeviceType\"@
    --
    -- -   @\"metadata.osName\": \"OSName\"@
    --
    -- -   @\"metadata.countryCode\": \"CountryCode\"@
    --
    -- -   @\"event_details.fileType\": \"FileType\"@
    --
    -- All dimensions listed in this field must also be included in
    -- @EventPattern@.
    dimensionKeys :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The pattern that defines the metric, specified as a JSON object. RUM
    -- checks events that happen in a user\'s session against the pattern, and
    -- events that match the pattern are sent to the metric destination.
    --
    -- When you define extended metrics, the metric definition is not valid if
    -- @EventPattern@ is omitted.
    --
    -- Example event patterns:
    --
    -- -   @\'{ \"event_type\": [\"com.amazon.rum.js_error_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Safari\" ], } }\'@
    --
    -- -   @\'{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Firefox\" ] }, \"event_details\": { \"duration\": [{ \"numeric\": [ \"\<\", 2000 ] }] } }\'@
    --
    -- -   @\'{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Safari\" ], \"countryCode\": [ \"US\" ] }, \"event_details\": { \"duration\": [{ \"numeric\": [ \">=\", 2000, \"\<\", 8000 ] }] } }\'@
    --
    -- If the metrics destination\' is @CloudWatch@ and the event also matches
    -- a value in @DimensionKeys@, then the metric is published with the
    -- specified dimensions.
    eventPattern :: Prelude.Maybe Prelude.Text,
    -- | The CloudWatch metric unit to use for this metric. If you omit this
    -- field, the metric is recorded with no unit.
    unitLabel :: Prelude.Maybe Prelude.Text,
    -- | The field within the event object that the metric value is sourced from.
    --
    -- If you omit this field, a hardcoded value of 1 is pushed as the metric
    -- value. This is useful if you just want to count the number of events
    -- that the filter catches.
    --
    -- If this metric is sent to CloudWatch Evidently, this field will be
    -- passed to Evidently raw and Evidently will handle data extraction from
    -- the event.
    valueKey :: Prelude.Maybe Prelude.Text,
    -- | The name for the metric that is defined in this structure. Valid values
    -- are the following:
    --
    -- -   @PerformanceNavigationDuration@
    --
    -- -   @PerformanceResourceDuration @
    --
    -- -   @NavigationSatisfiedTransaction@
    --
    -- -   @NavigationToleratedTransaction@
    --
    -- -   @NavigationFrustratedTransaction@
    --
    -- -   @WebVitalsCumulativeLayoutShift@
    --
    -- -   @WebVitalsFirstInputDelay@
    --
    -- -   @WebVitalsLargestContentfulPaint@
    --
    -- -   @JsErrorCount@
    --
    -- -   @HttpErrorCount@
    --
    -- -   @SessionCount@
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDefinitionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionKeys', 'metricDefinitionRequest_dimensionKeys' - Use this field only if you are sending the metric to CloudWatch.
--
-- This field is a map of field paths to dimension names. It defines the
-- dimensions to associate with this metric in CloudWatch. Valid values for
-- the entries in this field are the following:
--
-- -   @\"metadata.pageId\": \"PageId\"@
--
-- -   @\"metadata.browserName\": \"BrowserName\"@
--
-- -   @\"metadata.deviceType\": \"DeviceType\"@
--
-- -   @\"metadata.osName\": \"OSName\"@
--
-- -   @\"metadata.countryCode\": \"CountryCode\"@
--
-- -   @\"event_details.fileType\": \"FileType\"@
--
-- All dimensions listed in this field must also be included in
-- @EventPattern@.
--
-- 'eventPattern', 'metricDefinitionRequest_eventPattern' - The pattern that defines the metric, specified as a JSON object. RUM
-- checks events that happen in a user\'s session against the pattern, and
-- events that match the pattern are sent to the metric destination.
--
-- When you define extended metrics, the metric definition is not valid if
-- @EventPattern@ is omitted.
--
-- Example event patterns:
--
-- -   @\'{ \"event_type\": [\"com.amazon.rum.js_error_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Safari\" ], } }\'@
--
-- -   @\'{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Firefox\" ] }, \"event_details\": { \"duration\": [{ \"numeric\": [ \"\<\", 2000 ] }] } }\'@
--
-- -   @\'{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Safari\" ], \"countryCode\": [ \"US\" ] }, \"event_details\": { \"duration\": [{ \"numeric\": [ \">=\", 2000, \"\<\", 8000 ] }] } }\'@
--
-- If the metrics destination\' is @CloudWatch@ and the event also matches
-- a value in @DimensionKeys@, then the metric is published with the
-- specified dimensions.
--
-- 'unitLabel', 'metricDefinitionRequest_unitLabel' - The CloudWatch metric unit to use for this metric. If you omit this
-- field, the metric is recorded with no unit.
--
-- 'valueKey', 'metricDefinitionRequest_valueKey' - The field within the event object that the metric value is sourced from.
--
-- If you omit this field, a hardcoded value of 1 is pushed as the metric
-- value. This is useful if you just want to count the number of events
-- that the filter catches.
--
-- If this metric is sent to CloudWatch Evidently, this field will be
-- passed to Evidently raw and Evidently will handle data extraction from
-- the event.
--
-- 'name', 'metricDefinitionRequest_name' - The name for the metric that is defined in this structure. Valid values
-- are the following:
--
-- -   @PerformanceNavigationDuration@
--
-- -   @PerformanceResourceDuration @
--
-- -   @NavigationSatisfiedTransaction@
--
-- -   @NavigationToleratedTransaction@
--
-- -   @NavigationFrustratedTransaction@
--
-- -   @WebVitalsCumulativeLayoutShift@
--
-- -   @WebVitalsFirstInputDelay@
--
-- -   @WebVitalsLargestContentfulPaint@
--
-- -   @JsErrorCount@
--
-- -   @HttpErrorCount@
--
-- -   @SessionCount@
newMetricDefinitionRequest ::
  -- | 'name'
  Prelude.Text ->
  MetricDefinitionRequest
newMetricDefinitionRequest pName_ =
  MetricDefinitionRequest'
    { dimensionKeys =
        Prelude.Nothing,
      eventPattern = Prelude.Nothing,
      unitLabel = Prelude.Nothing,
      valueKey = Prelude.Nothing,
      name = pName_
    }

-- | Use this field only if you are sending the metric to CloudWatch.
--
-- This field is a map of field paths to dimension names. It defines the
-- dimensions to associate with this metric in CloudWatch. Valid values for
-- the entries in this field are the following:
--
-- -   @\"metadata.pageId\": \"PageId\"@
--
-- -   @\"metadata.browserName\": \"BrowserName\"@
--
-- -   @\"metadata.deviceType\": \"DeviceType\"@
--
-- -   @\"metadata.osName\": \"OSName\"@
--
-- -   @\"metadata.countryCode\": \"CountryCode\"@
--
-- -   @\"event_details.fileType\": \"FileType\"@
--
-- All dimensions listed in this field must also be included in
-- @EventPattern@.
metricDefinitionRequest_dimensionKeys :: Lens.Lens' MetricDefinitionRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
metricDefinitionRequest_dimensionKeys = Lens.lens (\MetricDefinitionRequest' {dimensionKeys} -> dimensionKeys) (\s@MetricDefinitionRequest' {} a -> s {dimensionKeys = a} :: MetricDefinitionRequest) Prelude.. Lens.mapping Lens.coerced

-- | The pattern that defines the metric, specified as a JSON object. RUM
-- checks events that happen in a user\'s session against the pattern, and
-- events that match the pattern are sent to the metric destination.
--
-- When you define extended metrics, the metric definition is not valid if
-- @EventPattern@ is omitted.
--
-- Example event patterns:
--
-- -   @\'{ \"event_type\": [\"com.amazon.rum.js_error_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Safari\" ], } }\'@
--
-- -   @\'{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Firefox\" ] }, \"event_details\": { \"duration\": [{ \"numeric\": [ \"\<\", 2000 ] }] } }\'@
--
-- -   @\'{ \"event_type\": [\"com.amazon.rum.performance_navigation_event\"], \"metadata\": { \"browserName\": [ \"Chrome\", \"Safari\" ], \"countryCode\": [ \"US\" ] }, \"event_details\": { \"duration\": [{ \"numeric\": [ \">=\", 2000, \"\<\", 8000 ] }] } }\'@
--
-- If the metrics destination\' is @CloudWatch@ and the event also matches
-- a value in @DimensionKeys@, then the metric is published with the
-- specified dimensions.
metricDefinitionRequest_eventPattern :: Lens.Lens' MetricDefinitionRequest (Prelude.Maybe Prelude.Text)
metricDefinitionRequest_eventPattern = Lens.lens (\MetricDefinitionRequest' {eventPattern} -> eventPattern) (\s@MetricDefinitionRequest' {} a -> s {eventPattern = a} :: MetricDefinitionRequest)

-- | The CloudWatch metric unit to use for this metric. If you omit this
-- field, the metric is recorded with no unit.
metricDefinitionRequest_unitLabel :: Lens.Lens' MetricDefinitionRequest (Prelude.Maybe Prelude.Text)
metricDefinitionRequest_unitLabel = Lens.lens (\MetricDefinitionRequest' {unitLabel} -> unitLabel) (\s@MetricDefinitionRequest' {} a -> s {unitLabel = a} :: MetricDefinitionRequest)

-- | The field within the event object that the metric value is sourced from.
--
-- If you omit this field, a hardcoded value of 1 is pushed as the metric
-- value. This is useful if you just want to count the number of events
-- that the filter catches.
--
-- If this metric is sent to CloudWatch Evidently, this field will be
-- passed to Evidently raw and Evidently will handle data extraction from
-- the event.
metricDefinitionRequest_valueKey :: Lens.Lens' MetricDefinitionRequest (Prelude.Maybe Prelude.Text)
metricDefinitionRequest_valueKey = Lens.lens (\MetricDefinitionRequest' {valueKey} -> valueKey) (\s@MetricDefinitionRequest' {} a -> s {valueKey = a} :: MetricDefinitionRequest)

-- | The name for the metric that is defined in this structure. Valid values
-- are the following:
--
-- -   @PerformanceNavigationDuration@
--
-- -   @PerformanceResourceDuration @
--
-- -   @NavigationSatisfiedTransaction@
--
-- -   @NavigationToleratedTransaction@
--
-- -   @NavigationFrustratedTransaction@
--
-- -   @WebVitalsCumulativeLayoutShift@
--
-- -   @WebVitalsFirstInputDelay@
--
-- -   @WebVitalsLargestContentfulPaint@
--
-- -   @JsErrorCount@
--
-- -   @HttpErrorCount@
--
-- -   @SessionCount@
metricDefinitionRequest_name :: Lens.Lens' MetricDefinitionRequest Prelude.Text
metricDefinitionRequest_name = Lens.lens (\MetricDefinitionRequest' {name} -> name) (\s@MetricDefinitionRequest' {} a -> s {name = a} :: MetricDefinitionRequest)

instance Data.FromJSON MetricDefinitionRequest where
  parseJSON =
    Data.withObject
      "MetricDefinitionRequest"
      ( \x ->
          MetricDefinitionRequest'
            Prelude.<$> (x Data..:? "DimensionKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EventPattern")
            Prelude.<*> (x Data..:? "UnitLabel")
            Prelude.<*> (x Data..:? "ValueKey")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable MetricDefinitionRequest where
  hashWithSalt _salt MetricDefinitionRequest' {..} =
    _salt `Prelude.hashWithSalt` dimensionKeys
      `Prelude.hashWithSalt` eventPattern
      `Prelude.hashWithSalt` unitLabel
      `Prelude.hashWithSalt` valueKey
      `Prelude.hashWithSalt` name

instance Prelude.NFData MetricDefinitionRequest where
  rnf MetricDefinitionRequest' {..} =
    Prelude.rnf dimensionKeys
      `Prelude.seq` Prelude.rnf eventPattern
      `Prelude.seq` Prelude.rnf unitLabel
      `Prelude.seq` Prelude.rnf valueKey
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON MetricDefinitionRequest where
  toJSON MetricDefinitionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DimensionKeys" Data..=) Prelude.<$> dimensionKeys,
            ("EventPattern" Data..=) Prelude.<$> eventPattern,
            ("UnitLabel" Data..=) Prelude.<$> unitLabel,
            ("ValueKey" Data..=) Prelude.<$> valueKey,
            Prelude.Just ("Name" Data..= name)
          ]
      )
