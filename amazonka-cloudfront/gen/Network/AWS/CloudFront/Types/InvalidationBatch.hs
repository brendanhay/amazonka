{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudFront.Types.InvalidationBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationBatch where

import Network.AWS.CloudFront.Types.Paths
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An invalidation batch.
--
-- /See:/ 'newInvalidationBatch' smart constructor.
data InvalidationBatch = InvalidationBatch'
  { -- | A complex type that contains information about the objects that you want
    -- to invalidate. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate>
    -- in the /Amazon CloudFront Developer Guide/.
    paths :: Paths,
    -- | A value that you specify to uniquely identify an invalidation request.
    -- CloudFront uses the value to prevent you from accidentally resubmitting
    -- an identical request. Whenever you create a new invalidation request,
    -- you must specify a new value for @CallerReference@ and change other
    -- values in the request as applicable. One way to ensure that the value of
    -- @CallerReference@ is unique is to use a @timestamp@, for example,
    -- @20120301090000@.
    --
    -- If you make a second invalidation request with the same value for
    -- @CallerReference@, and if the rest of the request is the same,
    -- CloudFront doesn\'t create a new invalidation request. Instead,
    -- CloudFront returns information about the invalidation request that you
    -- previously created with the same @CallerReference@.
    --
    -- If @CallerReference@ is a value you already sent in a previous
    -- invalidation batch request but the content of any @Path@ is different
    -- from the original request, CloudFront returns an
    -- @InvalidationBatchAlreadyExists@ error.
    callerReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InvalidationBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paths', 'invalidationBatch_paths' - A complex type that contains information about the objects that you want
-- to invalidate. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate>
-- in the /Amazon CloudFront Developer Guide/.
--
-- 'callerReference', 'invalidationBatch_callerReference' - A value that you specify to uniquely identify an invalidation request.
-- CloudFront uses the value to prevent you from accidentally resubmitting
-- an identical request. Whenever you create a new invalidation request,
-- you must specify a new value for @CallerReference@ and change other
-- values in the request as applicable. One way to ensure that the value of
-- @CallerReference@ is unique is to use a @timestamp@, for example,
-- @20120301090000@.
--
-- If you make a second invalidation request with the same value for
-- @CallerReference@, and if the rest of the request is the same,
-- CloudFront doesn\'t create a new invalidation request. Instead,
-- CloudFront returns information about the invalidation request that you
-- previously created with the same @CallerReference@.
--
-- If @CallerReference@ is a value you already sent in a previous
-- invalidation batch request but the content of any @Path@ is different
-- from the original request, CloudFront returns an
-- @InvalidationBatchAlreadyExists@ error.
newInvalidationBatch ::
  -- | 'paths'
  Paths ->
  -- | 'callerReference'
  Prelude.Text ->
  InvalidationBatch
newInvalidationBatch pPaths_ pCallerReference_ =
  InvalidationBatch'
    { paths = pPaths_,
      callerReference = pCallerReference_
    }

-- | A complex type that contains information about the objects that you want
-- to invalidate. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate>
-- in the /Amazon CloudFront Developer Guide/.
invalidationBatch_paths :: Lens.Lens' InvalidationBatch Paths
invalidationBatch_paths = Lens.lens (\InvalidationBatch' {paths} -> paths) (\s@InvalidationBatch' {} a -> s {paths = a} :: InvalidationBatch)

-- | A value that you specify to uniquely identify an invalidation request.
-- CloudFront uses the value to prevent you from accidentally resubmitting
-- an identical request. Whenever you create a new invalidation request,
-- you must specify a new value for @CallerReference@ and change other
-- values in the request as applicable. One way to ensure that the value of
-- @CallerReference@ is unique is to use a @timestamp@, for example,
-- @20120301090000@.
--
-- If you make a second invalidation request with the same value for
-- @CallerReference@, and if the rest of the request is the same,
-- CloudFront doesn\'t create a new invalidation request. Instead,
-- CloudFront returns information about the invalidation request that you
-- previously created with the same @CallerReference@.
--
-- If @CallerReference@ is a value you already sent in a previous
-- invalidation batch request but the content of any @Path@ is different
-- from the original request, CloudFront returns an
-- @InvalidationBatchAlreadyExists@ error.
invalidationBatch_callerReference :: Lens.Lens' InvalidationBatch Prelude.Text
invalidationBatch_callerReference = Lens.lens (\InvalidationBatch' {callerReference} -> callerReference) (\s@InvalidationBatch' {} a -> s {callerReference = a} :: InvalidationBatch)

instance Prelude.FromXML InvalidationBatch where
  parseXML x =
    InvalidationBatch'
      Prelude.<$> (x Prelude..@ "Paths")
      Prelude.<*> (x Prelude..@ "CallerReference")

instance Prelude.Hashable InvalidationBatch

instance Prelude.NFData InvalidationBatch

instance Prelude.ToXML InvalidationBatch where
  toXML InvalidationBatch' {..} =
    Prelude.mconcat
      [ "Paths" Prelude.@= paths,
        "CallerReference" Prelude.@= callerReference
      ]
