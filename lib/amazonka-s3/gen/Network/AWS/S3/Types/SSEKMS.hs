{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.SSEKMS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.SSEKMS where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Specifies the use of SSE-KMS to encrypt delivered inventory reports.
--
--
--
-- /See:/ 'sSEKMS' smart constructor.
newtype SSEKMS = SSEKMS' {_ssekKeyId :: Sensitive Text}
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'SSEKMS' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssekKeyId' - Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
sSEKMS ::
  -- | 'ssekKeyId'
  Text ->
  SSEKMS
sSEKMS pKeyId_ = SSEKMS' {_ssekKeyId = _Sensitive # pKeyId_}

-- | Specifies the ID of the AWS Key Management Service (AWS KMS) symmetric customer managed customer master key (CMK) to use for encrypting inventory reports.
ssekKeyId :: Lens' SSEKMS Text
ssekKeyId = lens _ssekKeyId (\s a -> s {_ssekKeyId = a}) . _Sensitive

instance FromXML SSEKMS where
  parseXML x = SSEKMS' <$> (x .@ "KeyId")

instance Hashable SSEKMS

instance NFData SSEKMS

instance ToXML SSEKMS where
  toXML SSEKMS' {..} = mconcat ["KeyId" @= _ssekKeyId]
