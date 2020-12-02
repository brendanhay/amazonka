{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the values for a task parameter.
--
--
--
-- /See:/ 'maintenanceWindowTaskParameterValueExpression' smart constructor.
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression'
  { _mwtpveValues ::
      Maybe
        ( Sensitive
            [ Sensitive
                Text
            ]
        )
  }
  deriving
    ( Eq,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'MaintenanceWindowTaskParameterValueExpression' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtpveValues' - This field contains an array of 0 or more strings, each 1 to 255 characters in length.
maintenanceWindowTaskParameterValueExpression ::
  MaintenanceWindowTaskParameterValueExpression
maintenanceWindowTaskParameterValueExpression =
  MaintenanceWindowTaskParameterValueExpression'
    { _mwtpveValues =
        Nothing
    }

-- | This field contains an array of 0 or more strings, each 1 to 255 characters in length.
mwtpveValues :: Lens' MaintenanceWindowTaskParameterValueExpression (Maybe [Text])
mwtpveValues = lens _mwtpveValues (\s a -> s {_mwtpveValues = a}) . mapping (_Sensitive . _Coerce)

instance FromJSON MaintenanceWindowTaskParameterValueExpression where
  parseJSON =
    withObject
      "MaintenanceWindowTaskParameterValueExpression"
      ( \x ->
          MaintenanceWindowTaskParameterValueExpression'
            <$> (x .:? "Values" .!= mempty)
      )

instance Hashable MaintenanceWindowTaskParameterValueExpression

instance NFData MaintenanceWindowTaskParameterValueExpression

instance ToJSON MaintenanceWindowTaskParameterValueExpression where
  toJSON MaintenanceWindowTaskParameterValueExpression' {..} =
    object (catMaybes [("Values" .=) <$> _mwtpveValues])
