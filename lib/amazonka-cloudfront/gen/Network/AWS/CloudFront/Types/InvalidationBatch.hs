{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationBatch where

import Network.AWS.CloudFront.Types.Paths
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An invalidation batch.
--
--
--
-- /See:/ 'invalidationBatch' smart constructor.
data InvalidationBatch = InvalidationBatch'
  { _ibPaths :: !Paths,
    _ibCallerReference :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InvalidationBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ibPaths' - A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
--
-- * 'ibCallerReference' - A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ . If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ . If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
invalidationBatch ::
  -- | 'ibPaths'
  Paths ->
  -- | 'ibCallerReference'
  Text ->
  InvalidationBatch
invalidationBatch pPaths_ pCallerReference_ =
  InvalidationBatch'
    { _ibPaths = pPaths_,
      _ibCallerReference = pCallerReference_
    }

-- | A complex type that contains information about the objects that you want to invalidate. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/Invalidation.html#invalidation-specifying-objects Specifying the Objects to Invalidate> in the /Amazon CloudFront Developer Guide/ .
ibPaths :: Lens' InvalidationBatch Paths
ibPaths = lens _ibPaths (\s a -> s {_ibPaths = a})

-- | A value that you specify to uniquely identify an invalidation request. CloudFront uses the value to prevent you from accidentally resubmitting an identical request. Whenever you create a new invalidation request, you must specify a new value for @CallerReference@ and change other values in the request as applicable. One way to ensure that the value of @CallerReference@ is unique is to use a @timestamp@ , for example, @20120301090000@ . If you make a second invalidation request with the same value for @CallerReference@ , and if the rest of the request is the same, CloudFront doesn't create a new invalidation request. Instead, CloudFront returns information about the invalidation request that you previously created with the same @CallerReference@ . If @CallerReference@ is a value you already sent in a previous invalidation batch request but the content of any @Path@ is different from the original request, CloudFront returns an @InvalidationBatchAlreadyExists@ error.
ibCallerReference :: Lens' InvalidationBatch Text
ibCallerReference = lens _ibCallerReference (\s a -> s {_ibCallerReference = a})

instance FromXML InvalidationBatch where
  parseXML x =
    InvalidationBatch'
      <$> (x .@ "Paths") <*> (x .@ "CallerReference")

instance Hashable InvalidationBatch

instance NFData InvalidationBatch

instance ToXML InvalidationBatch where
  toXML InvalidationBatch' {..} =
    mconcat
      ["Paths" @= _ibPaths, "CallerReference" @= _ibCallerReference]
