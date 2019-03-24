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
-- Module      : Network.AWS.SSM.LabelParameterVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A parameter label is a user-defined alias to help you manage different versions of a parameter. When you modify a parameter, Systems Manager automatically saves a new version and increments the version number by one. A label can help you remember the purpose of a parameter when there are multiple versions.
--
--
-- Parameter labels have the following requirements and restrictions.
--
--     * A version of a parameter can have a maximum of 10 labels.
--
--     * You can't attach the same label to different versions of the same parameter. For example, if version 1 has the label Production, then you can't attach Production to version 2.
--
--     * You can move a label from one version of a parameter to another.
--
--     * You can't create a label when you create a new parameter. You must attach a label to a specific version of a parameter.
--
--     * You can't delete a parameter label. If you no longer want to use a parameter label, then you must move it to a different version of a parameter.
--
--     * A label can have a maximum of 100 characters.
--
--     * Labels can contain letters (case sensitive), numbers, periods (.), hyphens (-), or underscores (_).
--
--     * Labels can't begin with a number, "aws," or "ssm" (not case sensitive). If a label fails to meet these requirements, then the label is not associated with a parameter and the system displays it in the list of InvalidLabels.
--
--
--
module Network.AWS.SSM.LabelParameterVersion
    (
    -- * Creating a Request
      labelParameterVersion
    , LabelParameterVersion
    -- * Request Lenses
    , lpvParameterVersion
    , lpvName
    , lpvLabels

    -- * Destructuring the Response
    , labelParameterVersionResponse
    , LabelParameterVersionResponse
    -- * Response Lenses
    , lpvrsInvalidLabels
    , lpvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'labelParameterVersion' smart constructor.
data LabelParameterVersion = LabelParameterVersion'
  { _lpvParameterVersion :: !(Maybe Integer)
  , _lpvName             :: !Text
  , _lpvLabels           :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelParameterVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvParameterVersion' - The specific version of the parameter on which you want to attach one or more labels. If no version is specified, the system attaches the label to the latest version.)
--
-- * 'lpvName' - The parameter name on which you want to attach one or more labels.
--
-- * 'lpvLabels' - One or more labels to attach to the specified parameter version.
labelParameterVersion
    :: Text -- ^ 'lpvName'
    -> NonEmpty Text -- ^ 'lpvLabels'
    -> LabelParameterVersion
labelParameterVersion pName_ pLabels_ =
  LabelParameterVersion'
    { _lpvParameterVersion = Nothing
    , _lpvName = pName_
    , _lpvLabels = _List1 # pLabels_
    }


-- | The specific version of the parameter on which you want to attach one or more labels. If no version is specified, the system attaches the label to the latest version.)
lpvParameterVersion :: Lens' LabelParameterVersion (Maybe Integer)
lpvParameterVersion = lens _lpvParameterVersion (\ s a -> s{_lpvParameterVersion = a})

-- | The parameter name on which you want to attach one or more labels.
lpvName :: Lens' LabelParameterVersion Text
lpvName = lens _lpvName (\ s a -> s{_lpvName = a})

-- | One or more labels to attach to the specified parameter version.
lpvLabels :: Lens' LabelParameterVersion (NonEmpty Text)
lpvLabels = lens _lpvLabels (\ s a -> s{_lpvLabels = a}) . _List1

instance AWSRequest LabelParameterVersion where
        type Rs LabelParameterVersion =
             LabelParameterVersionResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 LabelParameterVersionResponse' <$>
                   (x .?> "InvalidLabels") <*> (pure (fromEnum s)))

instance Hashable LabelParameterVersion where

instance NFData LabelParameterVersion where

instance ToHeaders LabelParameterVersion where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.LabelParameterVersion" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON LabelParameterVersion where
        toJSON LabelParameterVersion'{..}
          = object
              (catMaybes
                 [("ParameterVersion" .=) <$> _lpvParameterVersion,
                  Just ("Name" .= _lpvName),
                  Just ("Labels" .= _lpvLabels)])

instance ToPath LabelParameterVersion where
        toPath = const "/"

instance ToQuery LabelParameterVersion where
        toQuery = const mempty

-- | /See:/ 'labelParameterVersionResponse' smart constructor.
data LabelParameterVersionResponse = LabelParameterVersionResponse'
  { _lpvrsInvalidLabels  :: !(Maybe (List1 Text))
  , _lpvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LabelParameterVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpvrsInvalidLabels' - The label does not meet the requirements. For information about parameter label requirements, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling Parameters> in the /AWS Systems Manager User Guide/ .
--
-- * 'lpvrsResponseStatus' - -- | The response status code.
labelParameterVersionResponse
    :: Int -- ^ 'lpvrsResponseStatus'
    -> LabelParameterVersionResponse
labelParameterVersionResponse pResponseStatus_ =
  LabelParameterVersionResponse'
    {_lpvrsInvalidLabels = Nothing, _lpvrsResponseStatus = pResponseStatus_}


-- | The label does not meet the requirements. For information about parameter label requirements, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-labels.html Labeling Parameters> in the /AWS Systems Manager User Guide/ .
lpvrsInvalidLabels :: Lens' LabelParameterVersionResponse (Maybe (NonEmpty Text))
lpvrsInvalidLabels = lens _lpvrsInvalidLabels (\ s a -> s{_lpvrsInvalidLabels = a}) . mapping _List1

-- | -- | The response status code.
lpvrsResponseStatus :: Lens' LabelParameterVersionResponse Int
lpvrsResponseStatus = lens _lpvrsResponseStatus (\ s a -> s{_lpvrsResponseStatus = a})

instance NFData LabelParameterVersionResponse where
