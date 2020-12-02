{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.NoncurrentVersionExpiration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.NoncurrentVersionExpiration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Specifies when noncurrent object versions expire. Upon expiration, Amazon S3 permanently deletes the noncurrent object versions. You set this lifecycle configuration action on a bucket that has versioning enabled (or suspended) to request that Amazon S3 delete noncurrent object versions at a specific period in the object's lifetime.
--
--
--
-- /See:/ 'noncurrentVersionExpiration' smart constructor.
newtype NoncurrentVersionExpiration = NoncurrentVersionExpiration'
  { _nveNoncurrentDays ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NoncurrentVersionExpiration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nveNoncurrentDays' - Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
noncurrentVersionExpiration ::
  -- | 'nveNoncurrentDays'
  Int ->
  NoncurrentVersionExpiration
noncurrentVersionExpiration pNoncurrentDays_ =
  NoncurrentVersionExpiration'
    { _nveNoncurrentDays =
        pNoncurrentDays_
    }

-- | Specifies the number of days an object is noncurrent before Amazon S3 can perform the associated action. For information about the noncurrent days calculations, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html#non-current-days-calculations How Amazon S3 Calculates When an Object Became Noncurrent> in the /Amazon Simple Storage Service Developer Guide/ .
nveNoncurrentDays :: Lens' NoncurrentVersionExpiration Int
nveNoncurrentDays = lens _nveNoncurrentDays (\s a -> s {_nveNoncurrentDays = a})

instance FromXML NoncurrentVersionExpiration where
  parseXML x =
    NoncurrentVersionExpiration' <$> (x .@ "NoncurrentDays")

instance Hashable NoncurrentVersionExpiration

instance NFData NoncurrentVersionExpiration

instance ToXML NoncurrentVersionExpiration where
  toXML NoncurrentVersionExpiration' {..} =
    mconcat ["NoncurrentDays" @= _nveNoncurrentDays]
