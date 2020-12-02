{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionContentOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.LayerVersionContentOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
--
--
-- /See:/ 'layerVersionContentOutput' smart constructor.
data LayerVersionContentOutput = LayerVersionContentOutput'
  { _lvcoSigningProfileVersionARN ::
      !(Maybe Text),
    _lvcoLocation :: !(Maybe Text),
    _lvcoSigningJobARN :: !(Maybe Text),
    _lvcoCodeSize :: !(Maybe Integer),
    _lvcoCodeSha256 :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LayerVersionContentOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvcoSigningProfileVersionARN' - The Amazon Resource Name (ARN) for a signing profile version.
--
-- * 'lvcoLocation' - A link to the layer archive in Amazon S3 that is valid for 10 minutes.
--
-- * 'lvcoSigningJobARN' - The Amazon Resource Name (ARN) of a signing job.
--
-- * 'lvcoCodeSize' - The size of the layer archive in bytes.
--
-- * 'lvcoCodeSha256' - The SHA-256 hash of the layer archive.
layerVersionContentOutput ::
  LayerVersionContentOutput
layerVersionContentOutput =
  LayerVersionContentOutput'
    { _lvcoSigningProfileVersionARN =
        Nothing,
      _lvcoLocation = Nothing,
      _lvcoSigningJobARN = Nothing,
      _lvcoCodeSize = Nothing,
      _lvcoCodeSha256 = Nothing
    }

-- | The Amazon Resource Name (ARN) for a signing profile version.
lvcoSigningProfileVersionARN :: Lens' LayerVersionContentOutput (Maybe Text)
lvcoSigningProfileVersionARN = lens _lvcoSigningProfileVersionARN (\s a -> s {_lvcoSigningProfileVersionARN = a})

-- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
lvcoLocation :: Lens' LayerVersionContentOutput (Maybe Text)
lvcoLocation = lens _lvcoLocation (\s a -> s {_lvcoLocation = a})

-- | The Amazon Resource Name (ARN) of a signing job.
lvcoSigningJobARN :: Lens' LayerVersionContentOutput (Maybe Text)
lvcoSigningJobARN = lens _lvcoSigningJobARN (\s a -> s {_lvcoSigningJobARN = a})

-- | The size of the layer archive in bytes.
lvcoCodeSize :: Lens' LayerVersionContentOutput (Maybe Integer)
lvcoCodeSize = lens _lvcoCodeSize (\s a -> s {_lvcoCodeSize = a})

-- | The SHA-256 hash of the layer archive.
lvcoCodeSha256 :: Lens' LayerVersionContentOutput (Maybe Text)
lvcoCodeSha256 = lens _lvcoCodeSha256 (\s a -> s {_lvcoCodeSha256 = a})

instance FromJSON LayerVersionContentOutput where
  parseJSON =
    withObject
      "LayerVersionContentOutput"
      ( \x ->
          LayerVersionContentOutput'
            <$> (x .:? "SigningProfileVersionArn")
            <*> (x .:? "Location")
            <*> (x .:? "SigningJobArn")
            <*> (x .:? "CodeSize")
            <*> (x .:? "CodeSha256")
      )

instance Hashable LayerVersionContentOutput

instance NFData LayerVersionContentOutput
