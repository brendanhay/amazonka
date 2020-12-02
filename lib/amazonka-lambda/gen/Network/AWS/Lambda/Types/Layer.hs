{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Layer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Layer where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layer' smart constructor.
data Layer = Layer'
  { _lSigningProfileVersionARN :: !(Maybe Text),
    _lARN :: !(Maybe Text),
    _lSigningJobARN :: !(Maybe Text),
    _lCodeSize :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Layer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lSigningProfileVersionARN' - The Amazon Resource Name (ARN) for a signing profile version.
--
-- * 'lARN' - The Amazon Resource Name (ARN) of the function layer.
--
-- * 'lSigningJobARN' - The Amazon Resource Name (ARN) of a signing job.
--
-- * 'lCodeSize' - The size of the layer archive in bytes.
layer ::
  Layer
layer =
  Layer'
    { _lSigningProfileVersionARN = Nothing,
      _lARN = Nothing,
      _lSigningJobARN = Nothing,
      _lCodeSize = Nothing
    }

-- | The Amazon Resource Name (ARN) for a signing profile version.
lSigningProfileVersionARN :: Lens' Layer (Maybe Text)
lSigningProfileVersionARN = lens _lSigningProfileVersionARN (\s a -> s {_lSigningProfileVersionARN = a})

-- | The Amazon Resource Name (ARN) of the function layer.
lARN :: Lens' Layer (Maybe Text)
lARN = lens _lARN (\s a -> s {_lARN = a})

-- | The Amazon Resource Name (ARN) of a signing job.
lSigningJobARN :: Lens' Layer (Maybe Text)
lSigningJobARN = lens _lSigningJobARN (\s a -> s {_lSigningJobARN = a})

-- | The size of the layer archive in bytes.
lCodeSize :: Lens' Layer (Maybe Integer)
lCodeSize = lens _lCodeSize (\s a -> s {_lCodeSize = a})

instance FromJSON Layer where
  parseJSON =
    withObject
      "Layer"
      ( \x ->
          Layer'
            <$> (x .:? "SigningProfileVersionArn")
            <*> (x .:? "Arn")
            <*> (x .:? "SigningJobArn")
            <*> (x .:? "CodeSize")
      )

instance Hashable Layer

instance NFData Layer
