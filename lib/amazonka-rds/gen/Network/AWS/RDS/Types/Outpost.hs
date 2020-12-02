{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Outpost
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Outpost where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A data type that represents an Outpost.
--
--
-- For more information about RDS on Outposts, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Amazon RDS on AWS Outposts> in the /Amazon RDS User Guide./
--
--
-- /See:/ 'outpost' smart constructor.
newtype Outpost = Outpost' {_oARN :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Outpost' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oARN' - The Amazon Resource Name (ARN) of the Outpost.
outpost ::
  Outpost
outpost = Outpost' {_oARN = Nothing}

-- | The Amazon Resource Name (ARN) of the Outpost.
oARN :: Lens' Outpost (Maybe Text)
oARN = lens _oARN (\s a -> s {_oARN = a})

instance FromXML Outpost where
  parseXML x = Outpost' <$> (x .@? "Arn")

instance Hashable Outpost

instance NFData Outpost
