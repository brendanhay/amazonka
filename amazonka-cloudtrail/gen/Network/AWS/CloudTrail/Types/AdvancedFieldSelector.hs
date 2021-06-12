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
-- Module      : Network.AWS.CloudTrail.Types.AdvancedFieldSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.AdvancedFieldSelector where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A single selector statement in an advanced event selector.
--
-- /See:/ 'newAdvancedFieldSelector' smart constructor.
data AdvancedFieldSelector = AdvancedFieldSelector'
  { -- | An operator that excludes events that match the first few characters of
    -- the event record field specified as the value of @Field@.
    notStartsWith :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | An operator that excludes events that match the last few characters of
    -- the event record field specified as the value of @Field@.
    notEndsWith :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | An operator that excludes events that match the exact value of the event
    -- record field specified as the value of @Field@.
    notEquals :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | An operator that includes events that match the exact value of the event
    -- record field specified as the value of @Field@. This is the only valid
    -- operator that you can use with the @readOnly@, @eventCategory@, and
    -- @resources.type@ fields.
    equals :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | An operator that includes events that match the first few characters of
    -- the event record field specified as the value of @Field@.
    startsWith :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | An operator that includes events that match the last few characters of
    -- the event record field specified as the value of @Field@.
    endsWith :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | A field in an event record on which to filter events to be logged.
    -- Supported fields include @readOnly@, @eventCategory@, @eventSource@ (for
    -- management events), @eventName@, @resources.type@, and @resources.ARN@.
    --
    -- -   __@readOnly@__ - Optional. Can be set to @Equals@ a value of @true@
    --     or @false@. A value of @false@ logs both @read@ and @write@ events.
    --
    -- -   __@eventSource@__ - For filtering management events only. This can
    --     be set only to @NotEquals@ @kms.amazonaws.com@.
    --
    -- -   __@eventName@__ - Can use any operator. You can use it to ﬁlter in
    --     or ﬁlter out any data event logged to CloudTrail, such as
    --     @PutBucket@. You can have multiple values for this ﬁeld, separated
    --     by commas.
    --
    -- -   __@eventCategory@__ - This is required. It must be set to @Equals@,
    --     and the value must be @Management@ or @Data@.
    --
    -- -   __@resources.type@__ - This ﬁeld is required. @resources.type@ can
    --     only use the @Equals@ operator, and the value can be one of the
    --     following: @AWS::S3::Object@, @AWS::Lambda::Function@, or
    --     @AWS::S3Outposts::Object@. You can have only one @resources.type@
    --     ﬁeld per selector. To log data events on more than one resource
    --     type, add another selector.
    --
    -- -   __@resources.ARN@__ - You can use any operator with resources.ARN,
    --     but if you use @Equals@ or @NotEquals@, the value must exactly match
    --     the ARN of a valid resource of the type you\'ve speciﬁed in the
    --     template as the value of resources.type. For example, if
    --     resources.type equals @AWS::S3::Object@, the ARN must be in one of
    --     the following formats. The trailing slash is intentional; do not
    --     exclude it.
    --
    --     -   @arn:partition:s3:::bucket_name\/@
    --
    --     -   @arn:partition:s3:::bucket_name\/object_or_file_name\/@
    --
    --     When resources.type equals @AWS::Lambda::Function@, and the operator
    --     is set to @Equals@ or @NotEquals@, the ARN must be in the following
    --     format:
    --
    --     -   @arn:partition:lambda:region:account_ID:function:function_name@
    --
    --     When @resources.type@ equals @AWS::S3Outposts::Object@, and the
    --     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
    --     following format:
    --
    --     -   @arn:partition:s3-outposts:region:>account_ID:object_path@
    field :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AdvancedFieldSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notStartsWith', 'advancedFieldSelector_notStartsWith' - An operator that excludes events that match the first few characters of
-- the event record field specified as the value of @Field@.
--
-- 'notEndsWith', 'advancedFieldSelector_notEndsWith' - An operator that excludes events that match the last few characters of
-- the event record field specified as the value of @Field@.
--
-- 'notEquals', 'advancedFieldSelector_notEquals' - An operator that excludes events that match the exact value of the event
-- record field specified as the value of @Field@.
--
-- 'equals', 'advancedFieldSelector_equals' - An operator that includes events that match the exact value of the event
-- record field specified as the value of @Field@. This is the only valid
-- operator that you can use with the @readOnly@, @eventCategory@, and
-- @resources.type@ fields.
--
-- 'startsWith', 'advancedFieldSelector_startsWith' - An operator that includes events that match the first few characters of
-- the event record field specified as the value of @Field@.
--
-- 'endsWith', 'advancedFieldSelector_endsWith' - An operator that includes events that match the last few characters of
-- the event record field specified as the value of @Field@.
--
-- 'field', 'advancedFieldSelector_field' - A field in an event record on which to filter events to be logged.
-- Supported fields include @readOnly@, @eventCategory@, @eventSource@ (for
-- management events), @eventName@, @resources.type@, and @resources.ARN@.
--
-- -   __@readOnly@__ - Optional. Can be set to @Equals@ a value of @true@
--     or @false@. A value of @false@ logs both @read@ and @write@ events.
--
-- -   __@eventSource@__ - For filtering management events only. This can
--     be set only to @NotEquals@ @kms.amazonaws.com@.
--
-- -   __@eventName@__ - Can use any operator. You can use it to ﬁlter in
--     or ﬁlter out any data event logged to CloudTrail, such as
--     @PutBucket@. You can have multiple values for this ﬁeld, separated
--     by commas.
--
-- -   __@eventCategory@__ - This is required. It must be set to @Equals@,
--     and the value must be @Management@ or @Data@.
--
-- -   __@resources.type@__ - This ﬁeld is required. @resources.type@ can
--     only use the @Equals@ operator, and the value can be one of the
--     following: @AWS::S3::Object@, @AWS::Lambda::Function@, or
--     @AWS::S3Outposts::Object@. You can have only one @resources.type@
--     ﬁeld per selector. To log data events on more than one resource
--     type, add another selector.
--
-- -   __@resources.ARN@__ - You can use any operator with resources.ARN,
--     but if you use @Equals@ or @NotEquals@, the value must exactly match
--     the ARN of a valid resource of the type you\'ve speciﬁed in the
--     template as the value of resources.type. For example, if
--     resources.type equals @AWS::S3::Object@, the ARN must be in one of
--     the following formats. The trailing slash is intentional; do not
--     exclude it.
--
--     -   @arn:partition:s3:::bucket_name\/@
--
--     -   @arn:partition:s3:::bucket_name\/object_or_file_name\/@
--
--     When resources.type equals @AWS::Lambda::Function@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:partition:lambda:region:account_ID:function:function_name@
--
--     When @resources.type@ equals @AWS::S3Outposts::Object@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:partition:s3-outposts:region:>account_ID:object_path@
newAdvancedFieldSelector ::
  -- | 'field'
  Core.Text ->
  AdvancedFieldSelector
newAdvancedFieldSelector pField_ =
  AdvancedFieldSelector'
    { notStartsWith =
        Core.Nothing,
      notEndsWith = Core.Nothing,
      notEquals = Core.Nothing,
      equals = Core.Nothing,
      startsWith = Core.Nothing,
      endsWith = Core.Nothing,
      field = pField_
    }

-- | An operator that excludes events that match the first few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_notStartsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Core.Text))
advancedFieldSelector_notStartsWith = Lens.lens (\AdvancedFieldSelector' {notStartsWith} -> notStartsWith) (\s@AdvancedFieldSelector' {} a -> s {notStartsWith = a} :: AdvancedFieldSelector) Core.. Lens.mapping Lens._Coerce

-- | An operator that excludes events that match the last few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_notEndsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Core.Text))
advancedFieldSelector_notEndsWith = Lens.lens (\AdvancedFieldSelector' {notEndsWith} -> notEndsWith) (\s@AdvancedFieldSelector' {} a -> s {notEndsWith = a} :: AdvancedFieldSelector) Core.. Lens.mapping Lens._Coerce

-- | An operator that excludes events that match the exact value of the event
-- record field specified as the value of @Field@.
advancedFieldSelector_notEquals :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Core.Text))
advancedFieldSelector_notEquals = Lens.lens (\AdvancedFieldSelector' {notEquals} -> notEquals) (\s@AdvancedFieldSelector' {} a -> s {notEquals = a} :: AdvancedFieldSelector) Core.. Lens.mapping Lens._Coerce

-- | An operator that includes events that match the exact value of the event
-- record field specified as the value of @Field@. This is the only valid
-- operator that you can use with the @readOnly@, @eventCategory@, and
-- @resources.type@ fields.
advancedFieldSelector_equals :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Core.Text))
advancedFieldSelector_equals = Lens.lens (\AdvancedFieldSelector' {equals} -> equals) (\s@AdvancedFieldSelector' {} a -> s {equals = a} :: AdvancedFieldSelector) Core.. Lens.mapping Lens._Coerce

-- | An operator that includes events that match the first few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_startsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Core.Text))
advancedFieldSelector_startsWith = Lens.lens (\AdvancedFieldSelector' {startsWith} -> startsWith) (\s@AdvancedFieldSelector' {} a -> s {startsWith = a} :: AdvancedFieldSelector) Core.. Lens.mapping Lens._Coerce

-- | An operator that includes events that match the last few characters of
-- the event record field specified as the value of @Field@.
advancedFieldSelector_endsWith :: Lens.Lens' AdvancedFieldSelector (Core.Maybe (Core.NonEmpty Core.Text))
advancedFieldSelector_endsWith = Lens.lens (\AdvancedFieldSelector' {endsWith} -> endsWith) (\s@AdvancedFieldSelector' {} a -> s {endsWith = a} :: AdvancedFieldSelector) Core.. Lens.mapping Lens._Coerce

-- | A field in an event record on which to filter events to be logged.
-- Supported fields include @readOnly@, @eventCategory@, @eventSource@ (for
-- management events), @eventName@, @resources.type@, and @resources.ARN@.
--
-- -   __@readOnly@__ - Optional. Can be set to @Equals@ a value of @true@
--     or @false@. A value of @false@ logs both @read@ and @write@ events.
--
-- -   __@eventSource@__ - For filtering management events only. This can
--     be set only to @NotEquals@ @kms.amazonaws.com@.
--
-- -   __@eventName@__ - Can use any operator. You can use it to ﬁlter in
--     or ﬁlter out any data event logged to CloudTrail, such as
--     @PutBucket@. You can have multiple values for this ﬁeld, separated
--     by commas.
--
-- -   __@eventCategory@__ - This is required. It must be set to @Equals@,
--     and the value must be @Management@ or @Data@.
--
-- -   __@resources.type@__ - This ﬁeld is required. @resources.type@ can
--     only use the @Equals@ operator, and the value can be one of the
--     following: @AWS::S3::Object@, @AWS::Lambda::Function@, or
--     @AWS::S3Outposts::Object@. You can have only one @resources.type@
--     ﬁeld per selector. To log data events on more than one resource
--     type, add another selector.
--
-- -   __@resources.ARN@__ - You can use any operator with resources.ARN,
--     but if you use @Equals@ or @NotEquals@, the value must exactly match
--     the ARN of a valid resource of the type you\'ve speciﬁed in the
--     template as the value of resources.type. For example, if
--     resources.type equals @AWS::S3::Object@, the ARN must be in one of
--     the following formats. The trailing slash is intentional; do not
--     exclude it.
--
--     -   @arn:partition:s3:::bucket_name\/@
--
--     -   @arn:partition:s3:::bucket_name\/object_or_file_name\/@
--
--     When resources.type equals @AWS::Lambda::Function@, and the operator
--     is set to @Equals@ or @NotEquals@, the ARN must be in the following
--     format:
--
--     -   @arn:partition:lambda:region:account_ID:function:function_name@
--
--     When @resources.type@ equals @AWS::S3Outposts::Object@, and the
--     operator is set to @Equals@ or @NotEquals@, the ARN must be in the
--     following format:
--
--     -   @arn:partition:s3-outposts:region:>account_ID:object_path@
advancedFieldSelector_field :: Lens.Lens' AdvancedFieldSelector Core.Text
advancedFieldSelector_field = Lens.lens (\AdvancedFieldSelector' {field} -> field) (\s@AdvancedFieldSelector' {} a -> s {field = a} :: AdvancedFieldSelector)

instance Core.FromJSON AdvancedFieldSelector where
  parseJSON =
    Core.withObject
      "AdvancedFieldSelector"
      ( \x ->
          AdvancedFieldSelector'
            Core.<$> (x Core..:? "NotStartsWith")
            Core.<*> (x Core..:? "NotEndsWith")
            Core.<*> (x Core..:? "NotEquals")
            Core.<*> (x Core..:? "Equals")
            Core.<*> (x Core..:? "StartsWith")
            Core.<*> (x Core..:? "EndsWith")
            Core.<*> (x Core..: "Field")
      )

instance Core.Hashable AdvancedFieldSelector

instance Core.NFData AdvancedFieldSelector

instance Core.ToJSON AdvancedFieldSelector where
  toJSON AdvancedFieldSelector' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotStartsWith" Core..=) Core.<$> notStartsWith,
            ("NotEndsWith" Core..=) Core.<$> notEndsWith,
            ("NotEquals" Core..=) Core.<$> notEquals,
            ("Equals" Core..=) Core.<$> equals,
            ("StartsWith" Core..=) Core.<$> startsWith,
            ("EndsWith" Core..=) Core.<$> endsWith,
            Core.Just ("Field" Core..= field)
          ]
      )
