{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyOptionGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing option group.
--
--
module Network.AWS.RDS.ModifyOptionGroup
    (
    -- * Creating a Request
      modifyOptionGroup
    , ModifyOptionGroup
    -- * Request Lenses
    , mogOptionsToInclude
    , mogOptionsToRemove
    , mogApplyImmediately
    , mogOptionGroupName

    -- * Destructuring the Response
    , modifyOptionGroupResponse
    , ModifyOptionGroupResponse
    -- * Response Lenses
    , mogrsOptionGroup
    , mogrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'modifyOptionGroup' smart constructor.
data ModifyOptionGroup = ModifyOptionGroup'
  { _mogOptionsToInclude :: !(Maybe [OptionConfiguration])
  , _mogOptionsToRemove  :: !(Maybe [Text])
  , _mogApplyImmediately :: !(Maybe Bool)
  , _mogOptionGroupName  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyOptionGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mogOptionsToInclude' - Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
--
-- * 'mogOptionsToRemove' - Options in this list are removed from the option group.
--
-- * 'mogApplyImmediately' - Indicates whether the changes should be applied immediately, or during the next maintenance window for each instance associated with the option group.
--
-- * 'mogOptionGroupName' - The name of the option group to be modified. Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
modifyOptionGroup
    :: Text -- ^ 'mogOptionGroupName'
    -> ModifyOptionGroup
modifyOptionGroup pOptionGroupName_ =
  ModifyOptionGroup'
    { _mogOptionsToInclude = Nothing
    , _mogOptionsToRemove = Nothing
    , _mogApplyImmediately = Nothing
    , _mogOptionGroupName = pOptionGroupName_
    }


-- | Options in this list are added to the option group or, if already present, the specified configuration is used to update the existing configuration.
mogOptionsToInclude :: Lens' ModifyOptionGroup [OptionConfiguration]
mogOptionsToInclude = lens _mogOptionsToInclude (\ s a -> s{_mogOptionsToInclude = a}) . _Default . _Coerce

-- | Options in this list are removed from the option group.
mogOptionsToRemove :: Lens' ModifyOptionGroup [Text]
mogOptionsToRemove = lens _mogOptionsToRemove (\ s a -> s{_mogOptionsToRemove = a}) . _Default . _Coerce

-- | Indicates whether the changes should be applied immediately, or during the next maintenance window for each instance associated with the option group.
mogApplyImmediately :: Lens' ModifyOptionGroup (Maybe Bool)
mogApplyImmediately = lens _mogApplyImmediately (\ s a -> s{_mogApplyImmediately = a})

-- | The name of the option group to be modified. Permanent options, such as the TDE option for Oracle Advanced Security TDE, can't be removed from an option group, and that option group can't be removed from a DB instance once it is associated with a DB instance
mogOptionGroupName :: Lens' ModifyOptionGroup Text
mogOptionGroupName = lens _mogOptionGroupName (\ s a -> s{_mogOptionGroupName = a})

instance AWSRequest ModifyOptionGroup where
        type Rs ModifyOptionGroup = ModifyOptionGroupResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyOptionGroupResult"
              (\ s h x ->
                 ModifyOptionGroupResponse' <$>
                   (x .@? "OptionGroup") <*> (pure (fromEnum s)))

instance Hashable ModifyOptionGroup where

instance NFData ModifyOptionGroup where

instance ToHeaders ModifyOptionGroup where
        toHeaders = const mempty

instance ToPath ModifyOptionGroup where
        toPath = const "/"

instance ToQuery ModifyOptionGroup where
        toQuery ModifyOptionGroup'{..}
          = mconcat
              ["Action" =: ("ModifyOptionGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "OptionsToInclude" =:
                 toQuery
                   (toQueryList "OptionConfiguration" <$>
                      _mogOptionsToInclude),
               "OptionsToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _mogOptionsToRemove),
               "ApplyImmediately" =: _mogApplyImmediately,
               "OptionGroupName" =: _mogOptionGroupName]

-- | /See:/ 'modifyOptionGroupResponse' smart constructor.
data ModifyOptionGroupResponse = ModifyOptionGroupResponse'
  { _mogrsOptionGroup    :: !(Maybe OptionGroup)
  , _mogrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyOptionGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mogrsOptionGroup' - Undocumented member.
--
-- * 'mogrsResponseStatus' - -- | The response status code.
modifyOptionGroupResponse
    :: Int -- ^ 'mogrsResponseStatus'
    -> ModifyOptionGroupResponse
modifyOptionGroupResponse pResponseStatus_ =
  ModifyOptionGroupResponse'
    {_mogrsOptionGroup = Nothing, _mogrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mogrsOptionGroup :: Lens' ModifyOptionGroupResponse (Maybe OptionGroup)
mogrsOptionGroup = lens _mogrsOptionGroup (\ s a -> s{_mogrsOptionGroup = a})

-- | -- | The response status code.
mogrsResponseStatus :: Lens' ModifyOptionGroupResponse Int
mogrsResponseStatus = lens _mogrsResponseStatus (\ s a -> s{_mogrsResponseStatus = a})

instance NFData ModifyOptionGroupResponse where
