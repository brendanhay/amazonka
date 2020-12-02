{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NoncurrentVersionTransition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NoncurrentVersionTransition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TransitionStorageClass

-- | Container for the transition rule that describes when noncurrent objects transition to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class. If your bucket is versioning-enabled (or versioning is suspended), you can set this action to request that Amazon S3 transition noncurrent object versions to the @STANDARD_IA@ , @ONEZONE_IA@ , @INTELLIGENT_TIERING@ , @GLACIER@ , or @DEEP_ARCHIVE@ storage class at a specific period in the object's lifetime.
--
--
--
-- /See:/ 'noncurrentVersionTransition' smart constructor.
data NoncurrentVersionTransition = NoncurrentVersionTransition'
  { _nvtNoncurrentDays ::
      !Int,
    _nvtStorageClass ::
      !TransitionStorageClass
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NoncurrentVersionTransition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nvtNoncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'nvtStorageClass' - The class of storage used to store the object.
noncurrentVersionTransition ::
  -- | 'nvtNoncurrentDays'
  Int ->
  -- | 'nvtStorageClass'
  TransitionStorageClass ->
  NoncurrentVersionTransition
noncurrentVersionTransition pNoncurrentDays_ pStorageClass_ =
  NoncurrentVersionTransition'
    { _nvtNoncurrentDays =
        pNoncurrentDays_,
      _nvtStorageClass = pStorageClass_
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates How Long an Object Has Been Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
nvtNoncurrentDays :: Lens' NoncurrentVersionTransition Int
nvtNoncurrentDays = lens _nvtNoncurrentDays (\s a -> s {_nvtNoncurrentDays = a})

-- | The class of storage used to store the object.
nvtStorageClass :: Lens' NoncurrentVersionTransition TransitionStorageClass
nvtStorageClass = lens _nvtStorageClass (\s a -> s {_nvtStorageClass = a})

instance FromXML NoncurrentVersionTransition where
  parseXML x =
    NoncurrentVersionTransition'
      <$> (x .@ "NoncurrentDays") <*> (x .@ "StorageClass")

instance Hashable NoncurrentVersionTransition

instance NFData NoncurrentVersionTransition

instance ToXML NoncurrentVersionTransition where
  toXML NoncurrentVersionTransition' {..} =
    mconcat
      [ "NoncurrentDays" @= _nvtNoncurrentDays,
        "StorageClass" @= _nvtStorageClass
      ]
